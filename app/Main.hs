{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import DB
import Types
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Reader

withConn :: 
main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='code-consortium'" 
    pure ()
