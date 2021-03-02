{-# LANGUAGE OverloadedLists #-}
module Utils where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe
import Text.Read (readMaybe)
import Discord.Internal.Types.User qualified as U
import Types
import Data.Aeson

-- TODO make this less garbage
genUserGroup :: U.User -> GroupId
genUserGroup user = GroupId 
                  $ fromMaybe 0 (readMaybe (T.unpack (U.userDiscrim user))) `mod` 20
                    
tShow :: (Show a) => a -> Text
tShow = T.pack . show

compareSolutions :: JSONType -> Text -> Text -> Bool
compareSolutions t sol ans = fromMaybe False $ do
    pure True 

compareJSONType :: JSONType -> Value -> Bool
compareJSONType NumT (Number n) = True
compareJSONType StrT (String s) = True
compareJSONType (Arr a) (Array b) = all (compareJSONType a) b
-- is all rly necessary here? 
-- e.g. if you have Array [Number 1, String 'hello', etc], would lead to a false positive

tst :: Value
tst = Array [Array[Number 5]]