{-# LANGUAGE OverloadedLists, FlexibleInstances, UndecidableInstances #-}
module Utils 
( TShow (..)
, genUserGroup
, compareSolutions
, compareJSONType
, listToEither
, assertCond
, assertCondS
, liftMaybe
, liftMaybeS
, asDefaultWith
)where

import Data.Text qualified as T
import Discord.Internal.Types.User qualified as U
import Types
import Errors 
import CommonModules
import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy.Encoding qualified as TLE

-- TODO make this less garbage
genUserGroup :: U.User -> GroupId
genUserGroup user = GroupId 
                  $ fromMaybe 0 (readMaybe (T.unpack (U.userDiscrim user))) `mod` 20
                    
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
-- e.g. if we have Array [Number 1, String 'hello', etc], would lead to a false positive
-- we could probably use the Eq instance for Aeson.Value, but having the expected type stored with 
    -- the problem is also pretty useful for UX

listToEither :: a -> [b] -> Either a b
listToEither a = maybe (Left a) Right  . listToMaybe

-- create some specific instances since we use tShow a lot
class (Show a) => TShow a where
    tShow :: a -> Text
    tShow = pack . show

instance {-# OVERLAPPABLE #-} (Show a) => TShow a

instance {-# OVERLAPPABLE #-} TShow a => TShow [a] where
    tShow = T.concat . map tShow

instance TShow Text where
    tShow = id

instance TShow B.ByteString where
    tShow = decodeUtf8

instance  TShow BL.ByteString where
    tShow = tShow . BL.toStrict

instance  TShow [Char] where
    tShow = pack

_assertCond :: Applicative f => (t -> f ()) -> t -> Bool -> f ()
_assertCond thrower err = bool (thrower err) (pure ()) 

assertCond :: Monad m => e -> Bool -> ExceptT e m ()
assertCond = _assertCond throwE

-- this should probably be in Errors, but that would mean export _assertCond
assertCondS :: SubmissionError -> Bool -> SubHandler ()
assertCondS = _assertCond throwS

liftMaybe :: (Monad m) => e -> Maybe a -> ExceptT e m a
liftMaybe e = maybeToExceptT e . MaybeT . pure

liftMaybeS :: SubmissionError -> Maybe a -> SubHandler a
liftMaybeS e = SubHandler . liftMaybe e

-- I am out of good name ideas
asDefaultWith :: Monad m => m b -> Maybe a -> (a -> m b) -> m b 
asDefaultWith err mb act = maybe err act mb
-- asDefaultWith e = flip (maybe e)