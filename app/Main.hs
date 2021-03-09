{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Main where

import Bot.Handler
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.PostgreSQL.Simple ( close, connectPostgreSQL, Connection )
    
import DB
import CommonModules
import Discord (runDiscord)
import Lib
import System.Environment
import Types
import Utils

withConn :: ByteString -> (Connection -> IO ()) -> IO ()
withConn url f = do
  conn <- connectPostgreSQL url
  f conn
  close conn

loadDotEnv :: FilePath -> IO [(Text, Text)]
loadDotEnv s = do
    fp <- T.lines . pack <$> readFile s
    let mbPair [] = Nothing
        mbPair (t:ts) = Just (t, T.concat ts)
    pure $ mapMaybe (mbPair . T.split (== '=')) fp

listOrEnv ::  Text -> [(Text, Text)] -> IO (Maybe Text)
listOrEnv a = maybe (fmap pack <$> lookupEnv (T.unpack a)) (pure . Just) . lookup a 

getBotInfo :: IO (Maybe (Text, UserId))
getBotInfo = do
  args <- getArgs
  runMaybeT do 
    file <- MaybeT . pure $ listToMaybe args
    vars <- liftIO $ loadDotEnv file
    dst <- MaybeT (listOrEnv "DISCORD_TOKEN" vars)
    idStr <- T.unpack <$> MaybeT (listOrEnv "DISCORD_ID" vars) 
    dsId <- MaybeT . pure $ readMaybe idStr
    -- MaybeT $ pure (lookup "DISCORD_TOKEN" vars) 
    --   <|> (fmap pack <$> lookupEnv "DISCORD_TOKEN") -- these strings are different types
    pure (dst, dsId)

main :: IO ()
main = do
  -- fail if env var doesn't exist, we need this value
  maybeToken <- getBotInfo
  let noToken = error "You need to provide a Discord bot token and ID to run a Discord bot..."
  noToken `asDefaultWith` maybeToken $ \(dsTok, dsId) -> do
    withConn "dbname='code-consortium'" $ 
      -- runDiscord . buildBotOpts dsTok . (`BotEnv` dsId) >=> TIO.putStrLn
      runDiscord . buildBotOpts dsTok . BotEnv >=> TIO.putStrLn
