{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Text (pack)
import Data.Text.IO qualified as TIO
import Database.PostgreSQL.Simple
import Control.Monad
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Discord ( runDiscord )
import System.Environment

import Lib
import DB
import Types
import Utils
import Commands.Handler

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