{-# LANGUAGE DeriveGeneric, GADTs #-}
module Errors where
import GHC.Generics ( Generic )
import Types
import Text.Printf ( printf )

-- using GADTs so these are more extensible in the future
data SubmissionError where
    UserNotFound :: SubmissionError
    DoubleSignup :: SubmissionError
    InvalidInput :: SubmissionError
    ProbNotFound :: SubmissionError
    TypeMisMatch :: JSONType -> SubmissionError
    DBError      :: SubmissionError 
    WrongAnswer  :: SubmissionError
    deriving (Eq, Generic)

instance Show SubmissionError where
    show = ("Error: " <>) . \case
        UserNotFound   -> "Seems like you aren't signed up yet."
        ProbNotFound   -> "Invalid problem ID."
        InvalidInput   -> "Your submission is invalid - make sure they're formatted as valid JSON."
        TypeMisMatch t -> printf "Incorrect solution type - this problem is looking for a value of type %s" (show t) 
        DBError        -> "Something's wrong on our end, and we couldn't save your response. Try again in a few minutes :("
        WrongAnswer    -> "That answer is incorrect, keep at it though :D"
        DoubleSignup   -> "You're already signed up!"

type SubmissionResult = Either SubmissionError