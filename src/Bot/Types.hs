{-# LANGUAGE GADTs #-}
module Bot.Types where

import Types
import Data.Text ( Text )
-- no camelcase here, 
-- because the parsers depend on the string representations of these type constructors
data BotCmd = Submit 
            | Get
            | New
            | Input
            | Signup deriving (Eq, Show, Enum, Read)

data BotReq where
    SubmitR :: ProbId -> Text -> BotReq
    GetR :: ProbId -> BotReq
    NewR :: BotReq
    InputR :: BotReq
    SignupR :: BotReq deriving (Eq, Show)
