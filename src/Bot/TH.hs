{-# LANGUAGE TemplateHaskell #-}
module Bot.TH where

import Bot.Types
import Data.Char (toLower)
import Language.Haskell.TH.Syntax (lift)
import Text.Parsec

commandStrings :: [String]
commandStrings = $(lift (map (map toLower . show) $ enumFrom (toEnum 0 :: BotCmd)))

