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

getBotToken :: IO (Maybe Text)
getBotToken = do
  args <- getArgs
  runMaybeT do 
    file <- MaybeT . pure $ listToMaybe args
    vars <- liftIO $ loadDotEnv file
    MaybeT $ pure (lookup "DISCORD_TOKEN" vars) 
      <|> (fmap pack <$> lookupEnv "DISCORD_TOKEN") -- these strings are different types

main :: IO ()
main = do
  -- fail if env var doesn't exist, we need this value
  maybeToken <- getBotToken
  let noToken = error "You need to provide a Discord bot token to run a Discord bot..."
  noToken `asDefaultWith` maybeToken $ \dsTok -> do
    print dsTok
    withConn "dbname='code-consortium'" $ 
      runDiscord . buildBotOpts dsTok >=> TIO.putStrLn