{-# LANGUAGE TypeApplications, BlockArguments, RankNTypes #-}
module Bot.Handler where

import Control.Monad.Extra
import Data.Aeson (decodeStrict, Value (..))
import Data.Function
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Bifunctor (bimap, first)
import Data.Word
import Discord
import Discord.Internal.Types.User as U
import Discord.Requests
import Text.URI ( mkURI, URI, ParseException)
import Control.Monad.Catch as C

import CommonModules hiding (first)
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

buildBotOpts :: DSToken -> Connection -> RunDiscordOpts
buildBotOpts tok conn =
  def
    { discordToken = tok
    , discordOnEvent = eventHandler conn
    , discordOnLog = TIO.putStrLn
    , discordOnStart = liftIO $ putStrLn "CurryBot is running!"
    }

-- partially apply our connection to inject an environment
eventHandler :: Connection -> Event -> DiscordHandler ()
eventHandler conn = \case
  MessageCreate msg -> messageHandler conn msg
  _ -> pure ()

sentByHuman :: Message -> Bool
sentByHuman = not . liftA2 (||) userIsBot userIsWebhook . messageAuthor

respond :: Message -> Text -> DiscordHandler ()
respond msg txt = () <$ restCall (CreateMessage (messageChannel msg) txt)

respondEmbed :: Message -> (a -> CreateEmbed) -> a -> DiscordHandler () 
respondEmbed msg f a = () <$ (restCall . CreateMessageEmbed (messageChannel msg) "" $ f a)

-- Connection -> DBErr a -> SubHandler a
liftDB :: r -> ExceptT SubmissionError (ReaderT r IO) a -> SubHandler a
liftDB conn = liftSubmit . runDBErr conn

-- use ReaderT here?
type Responder a = Connection -> Message -> DiscordHandler a

messageHandler :: Responder ()
messageHandler conn msg = when (sentByHuman msg) $ do
  case parseMessage (messageText msg) of -- chances are we'll fail since every message is parsed
    Left _ -> pure ()
    Right cmd -> do
      liftIO (print cmd)
      let 
        f = case cmd of
          -- TODO accept user input as attachment too?
          SubmitR p t -> handleSubmit p t
          GetR p -> handleGet p
          NewR -> handleNew 
          InputR -> handleInput
          SignupR -> signup 
          HelpR -> helpMsg
      f conn msg

-- default show instance will drop the constructor when converting to string
fromSnowflake :: Snowflake -> Word64
fromSnowflake (Snowflake s) = s

handleSubmit :: ProbId -> Text -> Responder ()
handleSubmit pid ans conn msg = do
  res <- runSubmit $ do
    ans' <- fromUserSub @Text ans msg 

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
      user <- getUser . tShow . U.userId $ messageAuthor msg
      markSubmission pid user ans'
  let 
    fmtScore = printf "Congratulations! Your new score is %d" 
    respBody = tShow $ either show fmtScore res
  respond msg respBody

signup :: Responder ()
signup conn msg = do
  let author = messageAuthor msg 
      nope = "Error: You're already signed up!"
      yep  = "Signed up, welcome to the community!" -- tag user here?
  added <- runDB conn $ addUser (tShow $ U.userId author) (genUserGroup author)
  respond msg $ bool nope yep added

handleGet :: ProbId -> Responder ()
handleGet p conn msg = 
  runDBErr conn (getProbById p)
  >>= either 
      (respond msg . tShow) -- error condition
      (respondEmbed msg embedProblem) 

-- TODO finish these handlers!!!
handleInput :: Responder ()
handleInput conn msg = do
  result <- runSubmit $ do
    attach <- getAttachment msg
    pure undefined
  pure ()

-- TODO figure out a way to announce new problems, or implement some schedule
-- TODO restrict certain actions based on user role
handleNew :: Responder ()
handleNew conn msg = do
  result <- runSubmit $ do
    (ProbSub name desc typ) <- SubHandler 
      . liftMaybe InvalidInput 
      . decodeStrict @ProbSubmission 
      =<< getAttachment msg

    probId <- conn `liftDB` addProblem name desc typ
    let successMsg = printf 
          "New problem successfully added: `%s`. You can access it at problem `#%d`." name
    pure . pack $ successMsg probId
  respond msg $ either tShow id result

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