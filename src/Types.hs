{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Types where

import Data.Text qualified as T
import Data.ByteString.Lazy (fromStrict)
import Control.Lens.TH
import GHC.Generics
import Data.Aeson.TH
    ( deriveJSON,
      defaultOptions,
      Options(fieldLabelModifier, constructorTagModifier) )
import Data.Aeson (FromJSON (..), ToJSON (..), genericToEncoding, Value (..), decode')
import CommonModules hiding (User)

newtype ProbId = ProbId {getId :: Int} deriving (Eq, Show, Generic)
newtype GroupId = GroupId {getGrp :: Int} deriving (Eq, Show, Generic)

data JSONType = NumT | StrT | Arr JSONType deriving (Eq, Ord, Generic)

instance Show JSONType where
    show = \case
        StrT -> "string"
        NumT -> "number"
        Arr a -> "[" <> show a <> "]"

data Problem = Problem
    { _probId      :: ProbId
    , _probName    :: Text
    , _probInputs  :: Int
    , _probDesc    :: Text
    , _probSubmit  :: UTCTime
    , _probSolType :: JSONType
    } deriving (Eq, Show)

data User = User
    { _userId     :: Int
    , _userGroup  :: GroupId
    , _userName   :: Text
    , _userScore  :: Int
    , _userSolved :: Int
    } deriving (Eq, Show)

data Inputs = Inputs 
    { _inputJson :: Text -- don't need to decode
    , _groupId   :: GroupId
    , _answer    :: Text
    } deriving (Eq, Show)

makeLenses ''Problem
makeLenses ''User
makeLenses ''Inputs

-- using generics for automatic instances
instance FromJSON ProbId where
instance ToJSON ProbId where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GroupId where
instance ToJSON GroupId where
    toEncoding = genericToEncoding defaultOptions

-- can't abstract these options into a common thing within the same module
deriveJSON defaultOptions
    { fieldLabelModifier = drop 1
    , constructorTagModifier = ('_' :)
    } ''Problem

deriveJSON defaultOptions
    { fieldLabelModifier = drop 1
    , constructorTagModifier = ('_' :)
    } ''User

deriveJSON defaultOptions
    { fieldLabelModifier = drop 1
    , constructorTagModifier = ('_' :)
    } ''Inputs

is :: Eq b => (a -> b) -> b -> a -> Bool
is f p x = f x == p

-- toJSONType :: Text -> Maybe JSONType
-- toJSONType = decode' . fromStrict . encodeUtf8 

-- not worth writing parsec code for
toJSONType :: Text -> Maybe JSONType
toJSONType "number" = Just NumT
toJSONType "string" = Just StrT
toJSONType s | inBrackets = Arr <$> (toJSONType . T.init . T.tail) s
             | otherwise = Nothing
    where inBrackets = all ($ s) [not . T.null, T.head `is` '[', T.last `is` ']']
    
instance FromJSON JSONType where
    parseJSON = \case
        (String s) -> maybe empty pure $ toJSONType s
        _          -> empty

instance ToJSON JSONType where
    toJSON = String . \case 
        NumT -> "number"
        StrT -> "string"
        Arr x -> -- since Value is not a functor
            let (String x') = toJSON x
             in "[" <> x' <> "]"
