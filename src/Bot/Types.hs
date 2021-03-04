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

-- probably a more elegant solution with DataKinds
-- |Returns whether a specific bot request should have an attachment to be processed
takesAttachment :: BotReq -> Bool
takesAttachment = \case
    SubmitR _ _ -> True
    NewR        -> True 
    _           -> False

-- expand as needed
-- |Given a bot request, returns the inline data from the originating message if it exists
getInlineData :: BotReq -> Maybe Text
getInlineData = \case
    SubmitR _ t -> Just t
    _           -> Nothing