{-# LANGUAGE TypeApplications, BlockArguments, RankNTypes #-}
module Bot.Handler where

import Control.Monad.Extra
import Data.Function
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

-- use ReaderT here?
type Responder a = Connection -> Message -> DiscordHandler a

messageHandler :: Responder ()
messageHandler conn msg = when (sentByHuman msg) $ do
  case parseMessage (messageText msg) of -- chances are we'll fail since every message is parsed
    Left _ -> pure ()
    Right cmd -> do
      let 
        f = case cmd of
          -- TODO accept user input as attachment too?
          SubmitR p t -> handleSubmit p t 
          GetR p -> handleGet p
          NewR -> undefined
          InputR -> handleInput
          SignupR -> signup 
      f conn msg

-- default show instance will drop the constructor when converting to string
fromSnowflake :: Snowflake -> Word64
fromSnowflake (Snowflake s) = s

handleSubmit :: ProbId -> Text -> Responder ()
handleSubmit pid ans conn msg = do
  check <- runDBErr conn $ do
    user <- getUser (tShow . U.userId $ messageAuthor msg)
    markSubmission pid user ans
  let 
    fmtScore = printf "Congratulations! Your new score is %d" 
    respBody = tShow $ either show fmtScore check
  respond msg respBody

signup :: Responder ()
signup conn msg = do
  let author = messageAuthor msg 
      (uid, gid) = liftA2 (,) U.userId genUserGroup author
      nope = "ERROR: You're already signed up!"
      yep  = "Signed up, welcome to the community!" -- tag user here?
  added <- runDB conn $ addUser (tShow uid) gid
  respond msg $ bool nope yep added

handleGet :: ProbId -> Responder ()
handleGet p conn msg = runDBErr conn (getProbById p)
  >>= either 
      (respond msg . tShow) -- error condition
      (respondEmbed msg embedProblem) 
  
handleInput :: Responder ()
handleInput conn msg = pure ()

-- flip these args?
getMsgInput :: BotReq -> Message -> SubHandler Text
getMsgInput cmd msg 
  | takesAttachment cmd = maybe (getAttachment msg) pure $ getInlineData cmd
  | otherwise = throwS NoInput

dummyMsg t a = Message 0 0 dummyUsr t tme Nothing False False [] [] (maybeToList a) [] [] Nothing False Nothing
  where tme = read "2021-03-04 00:45:41.091531966 UTC"
dummyUsr = U.User 0 "" "" Nothing False False Nothing Nothing Nothing

dmt = dummyMsg "" . Just $ Attachment 0 "file.txt" 2048 "https://nliu.net/secret" "" Nothing Nothing

liftMaybeS :: SubmissionError -> Maybe a -> SubHandler a
liftMaybeS e = SubHandler . liftMaybe e

fetchAttachment :: MonadHttp m => Url a -> Option a -> m BsResponse
fetchAttachment url = req GET url NoReqBody bsResponse

getAttachment :: Message -> SubHandler Text
getAttachment msg = do
  attach <- exceptS . listToEither NoInput $ messageAttachments msg
  runExceptT $ assertCond ChonkyInput ((maxSubmissionSize >=) . attachmentSize) attach
  -- TODO ambiguous type variables, figure out how to use TypeApplications here
  let fromErr :: SubmissionError -> ParseException -> SubHandler a
      fromErr s = const (throwS s)
  parsed <- mkURI (attachmentUrl attach) `catch` fromErr InvalidInput
  urlOpts <- liftMaybeS InvalidInput (useHttpsURI parsed)
  -- runReq throws exceptions in rare situations, TODO figure out if it's worth trying to handle these
  -- if we can't contact discord this way, then we can't contact discord thru websockets

  -- update: even after lifting into a newtype, exceptions still fall through
  resp <- runReq defaultHttpConfig (uncurry fetchAttachment urlOpts)
            `catch` handleHttpException -- fromErr (NetworkError "your message attachment")
  pure . decodeUtf8 $ responseBody resp
  -- liftIO $ print parsed

  -- pure ""
  -- req GET url NoReqBody bsResponse mempty

test :: IO ()
test = do
  z <- print "hi"
  res <- runExceptT . runSubHandler $ getAttachment dmt
  print res
  pure ()
