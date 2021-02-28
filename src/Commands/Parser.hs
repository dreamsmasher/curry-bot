{-# LANGUAGE TemplateHaskell #-}
module Commands.Parser where

import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
-- import Text.Parser
import Data.Text qualified as T
import Data.Text (Text)
import Data.Char (toLower, toUpper, chr, ord)
import Commands.Types
import Data.Maybe

import Commands.TH

import Data.Functor
import Control.Applicative (liftA2, empty)
import Text.Read (readMaybe)
import Data.Bits (xor)

msgPrefix :: Char
msgPrefix = '$'

caseBlindC :: Char -> Parser Char
caseBlindC = liftA2 (<|>) char (char . chr . xor 32 . ord)
    -- ord ' ' = 32
    -- xor-ing any ASCII alpha char by 32 results in the flipped case

caseBlindS :: String -> Parser String
caseBlindS = traverse caseBlindC

p :: String -> IO ()
p = parseTest parseCommand 

capitalize :: String -> String
capitalize = mapCons toUpper toLower
    where mapCons _ _ [] = []
          mapCons f g (x:xs) = f x : map g xs

parseCommand :: Parser BotCmd
parseCommand = do 
    char msgPrefix
    s <- choice (map (try . caseBlindS) commandStrings) 
    maybe empty pure . readMaybe . capitalize $ s
    