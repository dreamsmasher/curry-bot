{-# LANGUAGE BlockArguments #-}
module Commands.Handler where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class ( MonadIO (..), liftIO )
import Control.Monad.Reader
import Data.Bool (bool)
import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import Data.Function
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word
import Database.PostgreSQL.Simple (Connection)
import Discord
import Discord.Internal.Types
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

type Responder a = Connection -> Message -> DiscordHandler a

messageHandler :: Responder ()
messageHandler conn msg = when (sentByHuman msg) $ do
  case parseMessage (messageText msg) of -- chances are we'll fail since every message is parsed
    Left _ -> pure ()
    Right cmd -> do
      let 
        f = case cmd of
          SubmitR p t -> handleSubmit p t 
          GetR p -> undefined
          NewR -> undefined
          InputR -> undefined
          SignupR -> signup 
      f conn msg

-- default show instance will drop the constructor
fromSnowflake :: Snowflake -> Word64
fromSnowflake (Snowflake s) = s

handleSubmit :: ProbId -> Text -> Responder ()
handleSubmit pid ans conn msg = do
  solved <- runMaybeT $ do
    user <- MaybeT (runDB conn $ getUser (tShow . U.userId . messageAuthor $ msg))
    pure (markSubmission pid user ans)
  pure ()
  -- whenJust probablyUser $ \user -> do
  --   pure ()

runDB :: MonadIO m => r -> ReaderT r IO a -> m a
runDB conn f = liftIO $ runReaderT f conn

signup :: Responder ()
signup conn msg = do
  let author = messageAuthor msg 
      uid  = U.userId author
      gid  = genUserGroup author
      nope = "ERROR: You're already signed up!"
      yep  = "Signed up, welcome to the community!"
  added <- runDB conn $ addUser (tShow uid) gid
  respond msg $ bool nope yep added
