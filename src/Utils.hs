{-# LANGUAGE OverloadedLists #-}
module Utils where

import Data.Text qualified as T
import Discord.Internal.Types.User qualified as U
import Types
import CommonModules
import Data.Aeson

-- TODO make this less garbage
genUserGroup :: U.User -> GroupId
genUserGroup user = GroupId 
                  $ fromMaybe 0 (readMaybe (T.unpack (U.userDiscrim user))) `mod` 20
                    
tShow :: (Show a) => a -> Text
tShow = T.pack . show

compareSolutions :: JSONType -> Text -> Text -> Bool
compareSolutions t sol ans = fromMaybe False $ do
    -- there's no direct Text -> Value function, afaik
    let toVal = decode . fromStrict . encodeUtf8
    solVal <- toVal sol 
    ansVal <- toVal ans
    pure $ compareJSONType t ansVal && solVal == ansVal

compareJSONType :: JSONType -> Value -> Bool
compareJSONType (Arr a) (Array b) = all (compareJSONType a) b
compareJSONType NumT (Number n) = True
compareJSONType StrT (String s) = True
compareJSONType _ _ = False
-- is `all` rly necessary here? 
-- e.g. if you have Array [Number 1, String 'hello', etc], would lead to a false positive
-- we could probably use the Eq instance for Aeson.Value, but having the expected type stored with 
-- the problem is also pretty useful for UX

listToEither :: a -> [b] -> Either a b
listToEither a = maybe (Left a) Right  . listToMaybe