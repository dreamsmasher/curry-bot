{-# LANGUAGE GADTs #-}
module Bot.Types where

import Types
import Data.Text qualified as T
import CommonModules

-- no camelcase here, 
-- because the parsers depend on the string representations of these type constructors
data BotCmd 
    = Submit 
    | Get
    | New
    | Input
    | Help
    | Signup deriving (Eq, Show, Enum, Read)

data BotReq 
    = SubmitR ProbId Text 
    | GetR ProbId 
    | NewR 
    | InputR 
    | HelpR
    | SignupR deriving (Eq, Show)

-- probably a more elegant solution with DataKinds
-- |Returns whether a specific bot request should have an attachment to be processed
takesAttachment :: BotReq -> Bool
takesAttachment = \case
    SubmitR _ _ -> True
    NewR        -> True 
    _           -> False

-- expand definitions as needed
-- |Given a bot request, returns the inline data from the originating message if it exists
getInlineData :: BotReq -> Maybe Text
getInlineData = \case
    SubmitR _ t -> bool Nothing (Just t) $ not (T.null t)
    --   | not $ T.null t -> Just t
    --   | otherwise = Nothing
    _           -> Nothing