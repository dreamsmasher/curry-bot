{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.Handler
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader
import DB
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Text.IO qualified as TIO
import Database.PostgreSQL.Simple ( close, connectPostgreSQL, Connection )
    
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

main :: IO ()
main = do
  -- fail if env var doesn't exist, we need this value
  dstok <- pack <$> getEnv "DISCORD_TOKEN"
  withConn "dbname='code-consortium'" 
    $ runDiscord . buildBotOpts dstok >=> TIO.putStrLn
   