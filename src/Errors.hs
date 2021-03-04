{-# LANGUAGE DeriveGeneric, GADTs, DeriveFunctor, GeneralisedNewtypeDeriving #-}
module Errors where
import GHC.Generics ( Generic )
import Types
import CommonModules
import Control.Monad.Trans.Class
import Bot.Constants
import Bot.TH (maxSubmissionErrorStr)
import Network.HTTP.Req
import Control.Monad.Catch ( Exception, MonadCatch, MonadThrow )

-- using GADTs so these are more extensible in the future
data SubmissionError 
    = UserNotFound  
    | DoubleSignup 
    | InvalidInput 
    | NoInput     
    | ProbNotFound 
    | TypeMisMatch JSONType 
    | DBError      
    | NetworkError Text 
    | WrongAnswer  
    | ChonkyInput  
    deriving (Eq, Generic)

instance Show SubmissionError where
    show = ("Error: " <>) . \case
        UserNotFound   -> "Seems like you aren't signed up yet."
        ProbNotFound   -> "Invalid problem ID."
        InvalidInput   -> "Your submission is invalid - make sure they're formatted as valid JSON."
        NoInput        -> "Entries need to either be encoded in the message, or attached as a JSON file."
        TypeMisMatch t -> printf "Incorrect solution type - this problem is looking for a value of type %s" (show t) 
        DBError        -> "Something's wrong on our end, and we couldn't save your response. Try again in a few minutes :("
        NetworkError s -> printf "I encountered an error while trying to fetch %s, sorry! Try again in a little bit." s
        WrongAnswer    -> "That answer is incorrect, keep at it though :D"
        DoubleSignup   -> "You're already signed up!"
        ChonkyInput    -> maxSubmissionErrorStr 

instance Exception SubmissionError

type SubmissionResult = Either SubmissionError

newtype SubHandler a = SubHandler {runSubHandler :: ExceptT SubmissionError IO a}  
    deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , MonadCatch
             , MonadIO
             , MonadFail
             )

instance MonadHttp SubHandler where            
    handleHttpException = const (SubHandler . throwE $ NetworkError "this resource")

throwS :: SubmissionError -> SubHandler a
catchS :: ExceptT e IO a -> (e -> ExceptT SubmissionError IO a) -> SubHandler a
exceptS :: Either SubmissionError a -> SubHandler a

throwS = SubHandler . throwE
catchS e = SubHandler . catchE e
exceptS = SubHandler . except

-- inverse functions
-- | run a SubHandler action in the IO monad.
runSubmit :: SubHandler a -> IO (SubmissionResult a)
runSubmit = runExceptT . runSubHandler

-- | turn an arbitrary IO (Either SubmissionResult a) action into a SubHandler.
liftSubmit :: IO (SubmissionResult a) -> SubHandler a
liftSubmit = SubHandler . ExceptT
