{-# LANGUAGE BlockArguments #-}
module Bot.Handler where

import Control.Monad.Extra
import Data.Function
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word
import Discord
import Discord.Internal.Types.User as U
import Discord.Requests

import CommonModules
import Types
import Errors
import DB
import Utils
import Bot.Parser
import Bot.Types
import Bot.Embed

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
          InputR -> undefined
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

  let fmtScore = printf "Congratulations! Your new score is %s" . tShow
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
  