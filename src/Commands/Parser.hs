{-# LANGUAGE OverloadedStrings #-}
module Commands.Parser where

-- import Text.Parsec
import Text.ParserCombinators.Parsec 
import Data.Text qualified as T
import Data.Text (Text)
import Data.Char (toLower, toUpper, chr, ord)
import Commands.Types
import Types
import Data.Maybe
import Data.Aeson

import Commands.TH

import Data.Functor
import Discord.Internal.Types.Channel (Message, Attachment)
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

parseProbId :: Parser ProbId
parseProbId = ProbId . read <$> many1 digit 

parseBotReq :: Parser BotReq
parseBotReq = do
    cmd <- parseCommand
    case cmd of
        Submit -> do
            spaces
            pid <- parseProbId
            spaces
            SubmitR pid . T.pack <$> manyTill anyChar eof
        Get -> spaces >> parseProbId <&> GetR
        New -> pure NewR
        Input -> pure InputR
        Signup -> pure SignupR
        -- these two are expected to be passed in as JSON

-- TODO: figure out if the constant packing/unpacking hurts performance
parseMessage :: Text -> Either ParseError BotReq
parseMessage = parse parseBotReq "" . T.unpack