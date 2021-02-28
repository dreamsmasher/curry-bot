{-# LANGUAGE DeriveGeneric, GADTs #-}
module Errors where
import GHC.Generics ( Generic )
import Types
import Text.Printf ( printf )

data SubmissionError where
    UserNotFound :: SubmissionError
    InvalidInput :: SubmissionError
    ProbNotFound :: SubmissionError
    deriving (Eq, Enum, Generic)

instance Show SubmissionError where
    show = ("Error: " <>) . \case
        UserNotFound -> "Seems like you aren't signed up yet."
        ProbNotFound -> "Invalid problem ID."
        InvalidInput -> "Your submission is invalid - make sure they're formatted as valid JSON."
