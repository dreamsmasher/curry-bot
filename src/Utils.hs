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
, asDefaultUsing
, mapEither
, showTrace
) where

import Data.Text qualified as T
import Debug.Trace ( trace )
import Discord.Internal.Types.User qualified as U
import Types
import Errors 
import CommonModules
import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Either (isRight)

-- TODO make this less garbage
genUserGroup :: U.User -> GroupId
genUserGroup user = GroupId 
                  $ fromMaybe 0 (readMaybe (T.unpack (U.userDiscrim user))) `mod` 20
                    
compareSolutions :: JSONType -> Value -> Value -> Bool
compareSolutions t sol = liftA2 (&&) (compareJSONType t) (sol ==) 

compareJSONType :: JSONType -> Value -> Bool
compareJSONType (Arr a) (Array b) = all (compareJSONType a) b
compareJSONType NumT (Number _) = True
compareJSONType StrT (String _) = True
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

instance TShow BL.ByteString where
    tShow = tShow . BL.toStrict
    -- TODO really expensive, refactor if this ends up getting used more

instance TShow [Char] where
    tShow = pack

-- useful for when we respond to a message either with an error msg from tShow or an actual thing
instance (TShow l, TShow r) => TShow (Either l r) where
    tShow = either tShow tShow

_assertCond :: Applicative f => (t -> f ()) -> t -> Bool -> f ()
_assertCond thrower err = bool (thrower err) (pure ()) 

assertCond :: Monad m => e -> Bool -> ExceptT e m ()
assertCond = _assertCond throwE

-- this should probably be in Errors, but that would mean exporting _assertCond
assertCondS :: SubmissionError -> Bool -> SubHandler ()
assertCondS = _assertCond throwS

liftMaybe :: (Monad m) => e -> Maybe a -> ExceptT e m a
liftMaybe e = maybeToExceptT e . MaybeT . pure

liftMaybeS :: SubmissionError -> Maybe a -> SubHandler a
liftMaybeS e = SubHandler . liftMaybe e

-- I am out of good name ideas
asDefaultUsing :: b -> Maybe a -> (a -> b) -> b 
asDefaultUsing err mb act = maybe err act mb
-- asDefaultUsing e = flip (maybe e)

-- |collect all the Right results from mapping a function that returns an Either value.
mapEither :: (a -> Either b c) -> [a] -> [c]
mapEither f = foldr (\x xs -> case f x of 
        Right x' -> x' : xs
        _ -> xs) []
    -- mapEither f = map (\(Right x) -> x) . filter isRight . map f

showTrace :: Show a => a -> a
showTrace = show >>= trace