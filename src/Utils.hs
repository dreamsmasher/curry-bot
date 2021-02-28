module Utils where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe
import Text.Read (readMaybe)

-- TODO make this less garbage
genUserGroup :: Text -> Int
genUserGroup user = let (_, s) = T.span (/= '#')  user in
    fromMaybe 0 ( readMaybe (T.unpack s)) `mod` 20