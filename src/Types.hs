{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Types where

import Data.Text ( Text )
import Data.Time ( UTCTime )
import Control.Lens.TH
import Data.Aeson.TH

data Problem = Problem
    { _probId     :: Int
    , _probName   :: Text
    , _probDesc   :: Text
    , _probSubmit :: UTCTime
    } deriving (Eq, Show)

data User = User
    { _userId     :: Int
    , _userName   :: Text
    , _userScore  :: Int
    , _userSolved :: Int
    } deriving (Eq, Show)

data Inputs = Inputs 
    { _inputJson :: Text -- don't need to decode
    , _answer :: Int
    } deriving (Eq, Show)

makeLenses ''Problem
makeLenses ''User
makeLenses ''Inputs

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
