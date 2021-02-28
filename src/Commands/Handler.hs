module Commands.Handler where

import Commands.Parser
import Commands.Types
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Database.PostgreSQL.Simple (Connection)
import Discord
import Discord.Internal.Types.Channel
import Discord.Internal.Types.Events
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User as U
import Discord.Requests
import Types

type DSToken = Text

buildBotOpts :: Connection -> DSToken -> RunDiscordOpts
buildBotOpts conn tok =
  def
    { discordToken = tok,
      discordOnEvent = eventHandler conn,
      discordOnLog = TIO.putStrLn
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
  case parseMessage (messageText msg) of
    Left err -> pure ()
    Right cmd -> case cmd of
      SubmitR p t -> undefined
      GetR p -> undefined
      NewR -> undefined
      InputR -> undefined
      SignupR -> undefined
