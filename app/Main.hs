{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import DB
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Types
import Utils
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)

withConn :: ByteString -> (Connection -> IO ()) -> IO ()
withConn url f = do
    conn <- connectPostgreSQL url
    f conn
    close conn

main :: IO ()
main = pure () -- withConn "dbname='code-consortium'" $ runReaderT $ do
    -- liftIO $ putStrLn "henlo!"
    -- let s = "Dreamsmasher#4819"
    -- -- ct <- addUser s (GroupId (genUserGroup s))
    -- -- liftIO (print ct)
    -- u <- getUser s
    -- sc <- updateScore (User 2 (GroupId 1) s 0 0) 100 
    -- u' <- getUser s
    -- liftIO (print u')
    -- liftIO (print sc)
    -- pure ()

    -- -- case u of
    -- --     Nothing -> liftIO (print "No user found")
    -- --     Just user -> do
    -- --         sc <- updateScore user 100 
    -- --         u' <- getUser s
    -- --         liftIO (print u')
    -- --         liftIO (print sc)
    -- --         pure ()
