{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Types where

import Data.Text ( Text )
import Data.Time ( UTCTime )
import Control.Monad.Trans.Reader
import Control.Lens.TH
import GHC.Generics
import Database.PostgreSQL.Simple ( Connection )
import Data.Aeson.TH
    ( deriveJSON,
      defaultOptions,
      Options(fieldLabelModifier, constructorTagModifier) )
import Data.Aeson (FromJSON, ToJSON (..), genericToEncoding)

-- represents a function that takes a DB connection


newtype ProbId = ProbId {getId :: Int} deriving (Eq, Show, Generic)
newtype GroupId = GroupId {getGrp :: Int} deriving (Eq, Show, Generic)

data Problem = Problem
    { _probId     :: ProbId
    , _probName   :: Text
    , _probInputs :: Int
    , _probDesc   :: Text
    , _probSubmit :: UTCTime
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
