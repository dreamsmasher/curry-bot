{-# LANGUAGE 
  DeriveGeneric
, DeriveFunctor
, FlexibleContexts
, FlexibleInstances
, MultiParamTypeClasses
, TemplateHaskell 
#-}

module Types where

import Data.Text qualified as T
import Data.ByteString.Lazy (fromStrict)
import Control.Lens.TH
import GHC.Generics
import Data.Char (toLower, toUpper)
import Data.Aeson.TH
    ( deriveJSON
    , defaultOptions
    , Options (..)
    , SumEncoding (..)
      )
import Data.Aeson 
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , genericToEncoding
    , (.:)
    , (.=)
    , decode'
    , encode
    , object
    )
import CommonModules hiding (User, (.=))

newtype ProbId = ProbId {getId :: Int} deriving (Eq, Show, Generic)
newtype GroupId = GroupId {getGrp :: Int} deriving (Eq, Show, Generic)

data JSONType = NumT | StrT | Arr !JSONType deriving (Eq, Ord, Generic)

instance Show JSONType where
    show = \case
        StrT -> "string"
        NumT -> "number"
        Arr a -> "[" <> show a <> "]"

-- direct map to our DB schema
data Problem = Problem
    { _probId      :: !ProbId
    , _probName    :: !Text
    , _probInputs  :: !Int
    , _probDesc    :: !Text
    , _probSubmit  :: {-# UNPACK #-} !UTCTime
    , _probSolType :: {-# UNPACK #-} !JSONType
    } deriving (Eq, Show)

data ProbSubmission = ProbSub
    { _PSubName :: !Text
    , _PSubDesc :: !Text
    , _PSubType :: {-# UNPACK #-} !JSONType
    } deriving (Eq, Show)

data User = User
    { _userId     :: !Int
    , _userGroup  :: !GroupId
    , _userName   :: !Text
    , _userScore  :: !Int
    , _userSolved :: !Int
    } deriving (Eq, Show)

data Inputs = Inputs 
    { _inputId   :: Maybe Int -- newtype this?
    , _groupId   :: !GroupId
    , _inputJson :: !Text -- don't need to decode
    , _answer    :: !Text
    } deriving (Eq, Show)

data InputSubmission = InputSub 
    { _inputSProbId :: !ProbId
    , _inputSJson   :: !Text
    , _inputSAns    :: !Text
    } deriving (Eq, Show, Generic)

data Plural a 
    = Single a
    | Many [a]
    deriving (Eq, Show, Generic, Functor)

makeLenses ''Problem
makeLenses ''ProbSubmission
makeLenses ''User
makeLenses ''Inputs
makeLenses ''InputSubmission

instance Foldable Plural where
    foldr f z = \case
        Single x -> f x z
        Many xs -> foldr f z xs

instance Traversable Plural where
    traverse f = \case
        Single a -> Single <$> f a
        Many xs -> Many <$> traverse f xs

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

deriveJSON defaultOptions 
    { fieldLabelModifier = drop 5 . map toLower
    , constructorTagModifier = ("PSub" <>) 
                             . (\case {[] -> []; (x:xs) -> toUpper x : map toLower xs})
    } ''ProbSubmission

instance FromJSON InputSubmission where
    parseJSON (Object v) = InputSub
        <$> (v .: "problemId" <&> ProbId) 
        <*>  v .: "input"
        <*>  v .: "answer"
    parseJSON _ = empty

instance ToJSON InputSubmission where
    toJSON (InputSub p i a) = object 
        [ "problemId" .= getId p
        , "input" .= i
        , "answer" .= a
        ]

deriveJSON defaultOptions 
  { sumEncoding = UntaggedValue
  } ''Plural
    -- parseJSON (Array a) = Many $ foldr (\x xs -> parseJSON x : xs) [] a

-- honestly belongs in Utils
is :: Eq b => (a -> b) -> b -> a -> Bool
is f p x = f x == p

-- toJSONType :: Text -> Maybe JSONType
-- toJSONType = decode' . fromStrict . encodeUtf8 

-- not worth writing parsec code for
toJSONType :: Text -> Maybe JSONType
toJSONType "number" = Just NumT
toJSONType "string" = Just StrT
toJSONType s | inBrackets s = Arr <$> (toJSONType . T.init . T.tail) s
             | otherwise = Nothing
    where all3 a b c = a && b && c
          inBrackets = liftA3 all3 (not . T.null) (T.head `is` '[') (T.last `is` ']')
          -- faster than using `all`
    
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

newtype BotEnv = BotEnv
    { _connection :: Connection
    -- , _botId :: UserId
    } deriving (Eq)

makeLenses ''BotEnv
