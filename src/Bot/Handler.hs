{-# LANGUAGE TypeApplications, BlockArguments, RankNTypes #-}
module Bot.Handler where

import Control.Monad.Extra
import Data.Aeson (decodeStrict, encode, Value (..), ToJSON (..), FromJSON (..))
import Data.Function
import Data.Foldable (toList)
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy ( toStrict )
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Bifunctor (bimap, first)
import Data.Tuple.Extra ( uncurry3 )
import Data.Word
import Data.Map qualified as M
import Discord
import Discord.Internal.Types.User hiding (User) 
import Discord.Internal.Types.User qualified as U
import Discord.Internal.Rest.Prelude
import Discord.Requests
import Text.URI ( mkURI, URI, ParseException)
import Control.Monad.Catch as C

import CommonModules hiding (first, User)
import Types
import Errors
import DB
import Utils
import Network.HTTP.Req
import Bot.Parser
import Bot.Types
import Bot.Embed
import Bot.Constants

type DSToken = Text

buildBotOpts :: DSToken -> BotEnv -> RunDiscordOpts
buildBotOpts tok env =
  def
    { discordToken = tok
    , discordOnEvent = eventHandler env
    , discordOnLog = TIO.putStrLn
    , discordOnStart = liftIO $ putStrLn "CurryBot is running!"
    }

-- partially apply our connection to inject an environment
eventHandler :: BotEnv -> Event -> DiscordHandler ()
eventHandler env = \case
  MessageCreate msg -> messageHandler env msg
  -- MessageReactionAdd r -> reactionHandler env r
  _ -> pure ()

sentByHuman :: Message -> Bool
sentByHuman = not . liftA2 (||) userIsBot userIsWebhook . messageAuthor

respond :: forall t. TShow t => Message -> t -> DiscordHandler ()
respond msg txt = () <$ restCall (CreateMessage (messageChannel msg) $ tShow txt)

respondEmbed :: Message -> (a -> CreateEmbed) -> a -> DiscordHandler () 
respondEmbed msg f = (() <$) .  restCall . CreateMessageEmbed (messageChannel msg) "" . f

-- JSON restriction is arbitrary, since we don't actually do any explicit conversions here
liftRest :: forall r a. (Request (r a), FromJSON a) => r a -> ExceptT RestCallErrorCode DiscordHandler a
liftRest = ExceptT . restCall

-- Connection -> DBErr a -> SubHandler a
liftDB :: r -> ExceptT SubmissionError (ReaderT r IO) a -> SubHandler a
liftDB conn = liftSubmit . runDBErr conn

-- use ReaderT here?
type Responder a = Connection -> Message -> DiscordHandler a

-- turns out we don't need to pass the userId in. Keeping the BotEnv record as our environment though.
getSelf :: DiscordHandler U.User
getSelf = _currentUser <$> readCache

getAllUsers :: GuildId -> DiscordHandler (Either RestCallErrorCode [U.User])
getAllUsers guild = fmap (map memberUser) 
                 <$> restCall (ListGuildMembers guild (GuildMembersTiming Nothing Nothing))

-- | Given a bot command, returns a handler function 
routeCmd :: BotReq -> (Connection -> Message -> DiscordHandler ())
routeCmd = \case
  -- TODO accept user input as attachment too?
  SubmitR p t -> handleSubmit p t
  GetR p -> handleGet p
  InputR p -> handleInput p
  Unary cmd' -> case cmd' of
    New -> handleNew 
    Addinput -> handleAddInput
    Signup -> signup 
    Help -> helpMsg
    Leaderboard -> handleLeaderboard
    _ -> error ("MISSING HANDLER FOR " <> show (Unary cmd')) 

messageHandler :: BotEnv -> Message -> DiscordHandler ()
messageHandler env msg = when (sentByHuman msg) do
  let conn = env ^. connection
  case parseMessage (messageText msg) of -- chances are we'll fail since every message is parsed
    Left _ -> pure ()
    Right cmd -> routeCmd cmd conn msg
      

-- default show instance will drop the constructor when converting to string
fromSnowflake :: Snowflake -> Word64
fromSnowflake (Snowflake s) = s

getUserFromMsg :: Message -> DBErr User
getUserFromMsg = getUser . tShow . U.userId . messageAuthor

handleSubmit :: ProbId -> Text -> Responder ()
handleSubmit pid ans conn msg = do
  res <- runSubmit do
    ans' <- fromUserSub @Value ans msg 

    {- this is actually a really funny transform
    we'll denote functions with angle brackets

    we go from 
      (ExceptT SubmissionError (ReaderT Connection IO) a)
        <runExceptT>->
        (ReaderT Connection IO (Either SubmissionError a))
          <`runReaderT` conn>->
          (IO (Either SubmissionError a))
          <liftIO>->
            ((MonadIO m) => m (Either SubmissionError a))
          <ExceptT>->
          (ExceptT SubmissionError IO a)
        <SubHandler>->
        (SubHandler (ExceptT SubmissionError IO a)) 
    
    altogether 5 monad transformations in a single line, within ReaderT DiscordHandle IO
    zero runtime cost, though :)
    -}
    conn `liftDB` do
      user <- getUserFromMsg msg
      markSubmission pid user ans'
  let fmtScore = printf "Congratulations! Your new score is %d!" :: Int -> String
  respond msg (fmtScore <$> res)

signup :: Responder ()
signup conn msg = do
  let author = messageAuthor msg 
      nope = "Error: You're already signed up!" :: Text
      yep  = "Signed up, welcome to the community!" -- tag user here?
  added <- runDB conn $ addUser (tShow $ U.userId author) (genUserGroup author)
  respond msg $ bool nope yep added

-- arguably the most expensive operation
-- involves 3 rest calls
sendInput :: (ToJSON a) => Problem -> Inputs a b -> Message -> DiscordHandler ()
sendInput p inp msg = do
  let fileStr = "currybot_input_%d.txt"
      probTxt = pack . printf fileStr . getId $ view probId p
      inpJson = toStrict $ encode (inp ^. inputJson)
      -- channel = messageChannel msg
  err <- runExceptT $ do
    -- get DM channel
    -- TODO maybe cache this with Redis?
    userChnl <- channelId <$> liftRest (CreateDM (U.userId $ messageAuthor msg))
    -- DM input to user
    sent <- liftRest $ CreateMessageUploadFile userChnl probTxt inpJson
    -- edit the message to embed the problem info
    liftRest $ EditMessage (userChnl, messageId sent) "" . Just $ embedProblem p
  when (isLeft err) $ liftIO (print err) 

handleInput :: Maybe ProbId -> Responder ()
handleInput Nothing _ msg = respond msg NoInput
handleInput (Just pid) conn msg = do
  probAndInput <- conn `runDBErr` do
    user <- getUserFromMsg msg
    -- TypeApplications don't work here for some reason
    problem <- getProbById pid
    input <- getUserInput user pid :: DBErr (Inputs Text Text)
    pure (problem, input)
  -- let resp = either (respond msg) (respondEmbed msg fmtInput)
  case probAndInput of 
    Left e -> respond msg e
    Right (prob, input) -> sendInput prob input msg

-- handleGet should DM a user when they react to it
handleGet :: Maybe ProbId -> Responder ()
handleGet Nothing _ msg = respond msg NoInput
handleGet (Just p) conn msg = 
  runDBErr conn (getProbById p)
  >>= either 
      (respond msg) -- error condition
      (respondEmbed msg embedProblem) 

handleAddInput :: Responder ()
handleAddInput conn msg = do
  result <- runSubmit do
    attach <- getAttachment msg
    parsed <- SubHandler . liftMaybe InvalidInput 
              $ decodeStrict @(Plural (InputSubmission Value Value)) attach
    conn `liftDB` do 
      let addFromInput (InputSub p j a) = addInputNoGid p j a
      and <$> traverse addFromInput parsed
  let respMsg = fmap 
        ( bool @Text
          "Some of those inputs weren't commited for some reason. Check back in a bit."
          "Inputs successfully added!"
        ) result
  respond msg respMsg

-- TODO another thing to cache
-- TODO figure out a better way to get every username, formatting them
handleLeaderboard :: Responder ()
handleLeaderboard conn msg = pure ()
  -- top <- conn `runDB` getLeaderBoard 
  -- let fmtTable :: [User] -> Text
  --     fmtTable xs = T.concat . (\lst -> ticks : (lst <> [ticks])) $ map (pack . uncurry3 (printf fmt)) userData
  --       where userData = map (\(User _ _ nme sc sv) -> (nme, show sc, show sv)) xs
  --             trimap f (a, b, c) = (f a, f b, f c)
  --             dotZip g (a, b, c) (d, e, f) = (g a b, g b e, g c f)
  --             (nme, sc, sv) = trimap (+ 4) $ foldl' (\a x -> dotZip max a (trimap length x)) (0, 0, 0) userData
  --             -- fmts@(nameFmt, scFmt, slvFmt) = trimap (printf "%% %dd" . (+ 4)) lens
  --             fmt = printf "%% %ds|%% %ds|%% %ds" nme sc sv :: String
  --             -- meta printf
  --             ticks = "```" :: Text
  -- respond msg $ fmtTable top 

-- TODO figure out a way to announce new problems, or implement some schedule
-- TODO restrict certain actions based on user role
handleNew :: Responder ()
handleNew conn msg = do
  result <- runSubmit do
    (ProbSub name desc typ) <- SubHandler 
      . liftMaybe InvalidInput 
      . decodeStrict @ProbSubmission 
      =<< getAttachment msg

    probId <- conn `liftDB` addProblem name desc typ
    let successMsg = printf 
          "New problem successfully added: `%s`. You can access it at problem `#%d`." name
    pure . pack $ successMsg probId
  respond msg result 

fromUserSub :: (FromJSON j) => Text -> Message -> SubHandler j
fromUserSub ans = getMsgInput ans 
  >=> decodeStrict >>> liftMaybe InvalidInput >>> SubHandler

-- flip these args?
getMsgInput :: Text -> Message -> SubHandler ByteString
getMsgInput body msg = 
  if not (T.null body)
    then pure $ encodeUtf8 body
    else getAttachment msg

-- TODO figure out if fetching the files will actually return decodable JSON
fetchAttachment :: forall m a. MonadHttp m => Url a -> Option a -> m BsResponse
fetchAttachment url = req GET url NoReqBody bsResponse

getAttachment :: Message -> SubHandler ByteString
getAttachment msg = do
  attach <- exceptS . listToEither NoInput $ messageAttachments msg
  assertCondS ChonkyInput $ attachmentSize attach <= maxSubmissionSize 
  let fromErr :: SubmissionError -> ParseException -> SubHandler a
      fromErr s _ = throwS s
  parsed <- mkURI (attachmentUrl attach) `catch` fromErr InvalidInput
  urlOpts <- liftMaybeS InvalidInput (useURI parsed)
  -- either Http, Https 

  let get = uncurry fetchAttachment
  resp <- runReq defaultHttpConfig (either get get urlOpts)
            `catch` handleHttpException
  pure $ responseBody resp

-- TODO maybe embed this?
helpMsg :: Responder ()
helpMsg _ msg = respond msg helpStr

-- Reaction handlers
-- TODO figure out a way to associate sent messages with reactions
-- pass around an MVar of MessageID's that the bot sent?
-- or just have users manually message the bot?
getMessageById :: ChannelId -> MessageId -> DiscordHandler (Either RestCallErrorCode Message)
getMessageById = curry $ restCall . GetChannelMessage 

-- reactionHandler :: BotEnv -> ReactionInfo -> DiscordHandler ()
-- reactionHandler env rxn = do
--   botId <- U.userId <$> getSelf -- todo figure out if this is worth keeping in BotEnv
--   liftIO $ print rxn
--   when (reactionUserId rxn == botId) do
--     runExceptT do
--       msg <- ExceptT $ getMessageById (reactionChannelId rxn) (reactionMessageId rxn)
--       pure ()
--     pure ()
   
--   pure ()
--   -- msg <- restCall ()
