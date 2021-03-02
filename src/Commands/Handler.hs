module Commands.Handler where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class ( MonadIO (..), liftIO )
import Control.Monad.Reader
import Data.Bool (bool)
import Data.Function
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.PostgreSQL.Simple (Connection)
import Discord
import Discord.Internal.Types.Channel
import Discord.Internal.Types.Events
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User as U
import Discord.Requests

import Types
import DB
import Utils
import Commands.Parser
import Commands.Types

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

messageHandler :: Connection -> Message -> DiscordHandler ()
messageHandler conn msg = when (sentByHuman msg) $ do
  case parseMessage (messageText msg) of -- chances are we'll fail since every message is parsed
    Left _ -> pure ()
    Right cmd -> case cmd of
      SubmitR p t -> undefined
      GetR p -> undefined
      NewR -> undefined
      InputR -> undefined
      SignupR -> signup conn msg

tShow :: (Show a) => a -> Text
tShow = T.pack . show

runDB :: MonadIO m => r -> ReaderT r IO a -> m a
runDB conn f = liftIO $ runReaderT f conn

signup :: Connection -> Message -> DiscordHandler ()
signup conn msg = do
  let author = messageAuthor msg 
      uid  = U.userId author
      gid  = genUserGroup author
      nope = "ERROR: You're already signed up!"
      yep  = "Signed up, welcome to the community!"
  added <- runDB conn $ addUser (tShow uid) gid
  respond msg $ bool nope yep added
