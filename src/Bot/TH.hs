{-# LANGUAGE TemplateHaskell #-}
module Bot.TH where

import Bot.Types
import Data.Char (toLower)
import Language.Haskell.TH.Syntax (lift)
import Text.Parsec
import Text.Printf
import Bot.Constants

commandStrings :: [String]
commandStrings = $(lift (map (map toLower . show) $ enumFrom (toEnum 0 :: BotCmd)))

maxSubmissionErrorStr :: String
maxSubmissionErrorStr = $( lift 
  $ (printf 
      "That input is way too large - you shouldn't need to submit anything > %dkB." 
      `asTypeOf` show)
  $ maxSubmissionSize `div` 1000)