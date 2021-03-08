{-# LANGUAGE BlockArguments, FlexibleContexts, RankNTypes #-}

module DB
( DBErr (..)
, runDB
, runDBErr
, problemSelect
, userSelect
, getAllProblems
, matchesId
, getProbById
, getInputsByProbGroup
, addUser
, addInput
, addInputNoGid
, addProblem
, getUser
, getUserInput
, verifySolution
, updateScore
, markSubmission
, clearInputs
) where

import Data.Aeson 
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int64)
import Data.Profunctor.Product
import Data.Profunctor
import Data.Tuple.Curry (Curry, uncurryN)
import Opaleye hiding (except)
import Types
import CommonModules hiding (User, userId)
import Bot.Constants
import Errors
import Utils (tShow, compareSolutions, listToEither, mapEither)

type DB = ReaderT Connection IO

type DBErr = ExceptT SubmissionError DB

-- |Given a database connection and action, run the action in any MonadIO context.
runDB :: MonadIO m => r -> ReaderT r IO a -> m a
runDB env = liftIO . (`runReaderT` env)

-- |Given a database connection and action that can fail (e.g. looking up by ID), run the action
-- in any MonadIO context, returning an Either type. Generally these actions have the form ExceptT SubmissionError (ReaderT Connection IO).
runDBErr :: MonadIO m => r -> ExceptT e (ReaderT r IO) a -> m (Either e a)
runDBErr env = runDB env . runExceptT

type ProbFieldsI =
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Maybe (Field SqlInt4)
  , Field SqlText
  , Maybe (Field SqlTimestamptz)
  , Field SqlText
  )

type ProbFieldsO =
  ( Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , Field SqlText
  , Field SqlTimestamptz
  , Field SqlText
  )

type ProbTable = Table ProbFieldsI ProbFieldsO

serialField :: String -> TableFields (Maybe (Column a)) (Column a)
serialField = optionalTableField

probTable :: ProbTable
probTable =
  table "problems" $ p6
      ( serialField "id"
      , tableField  "name"
      , serialField "n_inputs"
      , tableField  "description"
      , serialField "submitted_at"
      , tableField "solution_type"
      )

type UserFieldsI =
  ( Maybe (Field SqlInt4)
  , Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , Field SqlInt4
  )

type UserFieldsO =
  ( Field SqlInt4
  , Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , Field SqlInt4
  )

type UserTable = Table UserFieldsI UserFieldsO

userTable :: UserTable
userTable =
  table "users" $ p5
      ( serialField "id"
      , tableField  "group_id"
      , tableField  "snowflake"
      , tableField  "score"
      , tableField  "solved"
      )

type InputFieldI =
  ( Maybe (Field SqlInt4)
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlJson
  , Field SqlJson
  )

type InputFieldO =
  ( Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlJson
  , Field SqlJson
  )

type InputTable = Table InputFieldI InputFieldO

inputTable :: InputTable
inputTable =
  table "inputs" $ p5
      ( serialField "id"
      , tableField  "problem_id"
      , tableField  "group_id"
      , tableField  "input"
      , tableField  "answer"
      )

type AnswerField = 
  ( Field SqlInt4
  , Field SqlInt4
  , Field SqlInt4
  , Field SqlBool
  )

type AnswerTable = Table AnswerField AnswerField

answerTable :: AnswerTable
answerTable = 
  table "answers" $ p4
      ( tableField "problem_id"
      , tableField "input_id"
      , tableField "user_id"
      , tableField "solved"
      )

_where :: Column SqlBool -> Select ()
_where = viaLateral restrict

-- some wacky typeclass stuff happening in the background, would be nice to have this be generic
-- genericSel :: (c -> d) -> Select c -> DB [d]
-- genericSel constr sel = (map constr <$>) . (`runSelect` sel)

withConn :: (Connection -> IO b) -> DB b
withConn f = do
  conn <- ask
  liftIO (f conn)

toProblem :: (Int, Text, Int, Text, UTCTime, Text) -> Problem
toProblem = uncurryN (Problem . ProbId) . over _6 (fromMaybe NumT . toJSONType)

problemSelect :: Select ProbFieldsO -> DB [Problem]
problemSelect ps = withConn $ (map toProblem <$>) . (`runSelect` ps)

toUser :: (Int, Int, Text, Int, Int) -> User
toUser = uncurryN User . (_2' %~ GroupId)

userSelect :: Select UserFieldsO -> DB [User]
userSelect us = withConn $ (map toUser <$>) . (`runSelect` us)

answerSelect :: ((Int, Int, Int, Bool) -> a) -> Select AnswerField -> DB [a] 
answerSelect f as = withConn $ (map f <$>) . (`runSelect` as)

liftQuery :: Functor m => (a -> Either e b) -> (t -> m a) -> t -> ExceptT e m b
liftQuery modifier selector q = ExceptT $ modifier <$> selector q

runQueryOr :: Functor m => e -> (t -> m [a]) -> t -> ExceptT e m a
runQueryOr err = liftQuery (listToEither err)

tryDB :: (Connection -> b -> IO [a]) -> b -> DBErr a
tryDB queryFunc = runQueryOr DBError $ withConn . flip queryFunc

tryQuery :: (a -> Either e b1) -> (Connection -> b2 -> IO a) -> b2 -> ExceptT e (ReaderT Connection IO) b1
tryQuery modFunc queryFunc = liftQuery modFunc $ withConn . flip queryFunc

getUser :: Text -> DBErr User
getUser usr = runQueryOr UserNotFound userSelect q
  where
    q = do
      rows <- selectTable userTable
      let dsc = view _3 rows
      where_ (dsc .== sqlStrictText usr)
      pure rows

getAllProblems :: DB [Problem]
getAllProblems = problemSelect (selectTable probTable)

matchesId :: Column SqlInt4 -> Int -> Column SqlBool
matchesId a b = a .== sqlInt4 b

getProbById :: ProbId -> DBErr Problem
getProbById (ProbId p) = runQueryOr ProbNotFound problemSelect $ do
  rows <- selectTable probTable
  let probId = view _1 rows
  _where $ probId `matchesId` p
  pure rows

insertVal :: Table inRow outRow -> inRow -> DB Bool
insertVal t row = (0 <) <$> withConn (`runInsert_` i)
  where
    i = Insert
      { iTable = t
      , iRows = [row]
      , iReturning = rCount
      , iOnConflict = Just DoNothing
      }

fromRes :: Result a -> SubmissionResult a
fromRes (Error s) = Left InvalidInput 
fromRes (Success x) = Right x

fromValue :: forall a. FromJSON a => Value -> SubmissionResult a
fromValue = fromRes . fromJSON 

toInput :: (FromJSON a, FromJSON b) => (Int, Int, Int, Value, Value) -> SubmissionResult (Inputs a b)
toInput (i, p, g, j, a) = do
  j' <- fromValue j
  a' <- fromValue a
  pure $ Inputs (Just i) (ProbId p) (GroupId g) j' a'

getInputsByProbGroup :: forall a b. (FromJSON a, FromJSON b) => ProbId -> GroupId -> DBErr [Inputs a b]
getInputsByProbGroup (ProbId p) (GroupId g) = tryQuery (nonEmpty . mapEither toInput) runSelect q-- tryQuery nonEmpty sel q
  where 
    
    nonEmpty = \case
      [] -> Left NoAssocInput
      xs -> Right xs
    q = do
      rows@(_, probId, _, _, _) <- selectTable inputTable
      where_ (probId `matchesId` p)
      pure rows

assocAnswer :: (FromJSON a, FromJSON b) => User -> ProbId -> DBErr (Inputs a b)
assocAnswer usr pid = do
  let ug = usr ^. userGroup
  prob <- getProbById pid
  inputs <- getInputsByProbGroup pid ug
  let inp = nthModulo (prob ^. probInputs) (getGrp ug) inputs
      row = (sqlInt4 $ getId pid, sqlInt4 $ getGrp ug, sqlInt4 $ usr ^. userId, sqlBool False)
      ins = Insert 
          { iTable = answerTable
          , iRows = [row]
          , iReturning = rCount
          , iOnConflict = Just DoNothing
          }
  -- liftQuery (const (pure inp)) $ withConn $ map 
  ExceptT 
    $ bool (Left DBError) (Right inp) . (0 <) 
    <$> withConn (`runInsert_` ins)

getUserInput :: (ToJSON a, FromJSON a, ToJSON b, FromJSON b) => User -> ProbId -> DBErr (Inputs a b)
getUserInput usr pid = getExisting `catchE` (\_ -> assocAnswer usr pid)
-- if there isn't an answer association already, make one
  where 
    getExisting = do
      inpId <- tryDB runSelect do
        rows@(p, i, u, s) <- selectTable answerTable
        where_ (p .== sqlInt4 (getId pid) .&& u .== sqlInt4 (usr ^. userId))
        pure i
      except . toInput =<< tryDB runSelect do
        rows@(i, _, _, _, _) <- selectTable inputTable
        where_ (i .== sqlInt4 inpId)
        pure rows
      

-- worth adding errors here?
addUser :: Text -> GroupId -> DB Bool
addUser username (GroupId gid) = insertVal userTable
  (Nothing, sqlInt4 gid, sqlStrictText username, 0, 0)

addProblem :: Text -> Text -> JSONType -> DBErr Int
addProblem name desc ptype = tryDB runInsert_ i
  where row = (Nothing, sqlStrictText name, Nothing, sqlStrictText desc, Nothing, sqlStrictText $ tShow ptype)
        i = Insert
          { iTable = probTable
          , iRows = [row]
          , iReturning = rReturningI (view _1) -- should probably just extend insertVal
          , iOnConflict = Just DoNothing
          }

addInput :: (ToJSON a, ToJSON b) => ProbId -> GroupId -> a -> b -> DB Bool
addInput pid gid inpJson ans = insertVal inputTable
  ( Nothing
  , sqlInt4 . getId $ pid
  , sqlInt4 . getGrp $ gid
  , sqlValueJSON inpJson
  -- need to wrap the inner text in quotes, so we convert from Text -> String -> Bytestring
  -- definitely some unnecessary copying here 
  , sqlValueJSON ans
  )

addInputNoGid :: (FromJSON a, ToJSON a, FromJSON b, ToJSON b) => ProbId -> a -> b -> DBErr Bool
addInputNoGid pid json ans = do
  prob <- getProbById pid
  let g = GroupId (prob ^. probInputs)
  lift $ addInput pid (GroupId $ prob ^. probInputs) json ans

nthModulo :: Int -> Int -> [a] -> a
nthModulo len n = (!! (n `mod` len))

verifySolution :: ProbId -> User -> Text -> DBErr Bool
verifySolution probId user ans = do
  prob <- getProbById probId
  input <- getUserInput @Value user probId -- just decode as a value for JSON comparison
  pure $ compareSolutions (prob ^. probSolType) (input ^. answer) ans

-- update :: Update a -> DB
update :: forall a. Update a -> DB a
update args = withConn (`runUpdate_` args)

updateScore :: User -> Int -> DBErr Int
updateScore u sc = tryDB runUpdate_ uArgs 
  where
    -- these need explicit foralls in order to typecheck
    tupId :: forall s t a b. Field1 s t a b => Lens s t a b
    tupScore :: forall s t a b. Field4 s t a b => Lens s t a b
    tupId = _1'
    tupScore  = _4'
    tupSolved = _5'
    uArgs = Update
      { uTable = userTable
      , uReturning = rReturningI (view tupScore)
      , uUpdateWith = (tupId %~ Just) . (tupSolved %~ (+ 1)) . (tupScore %~ (fromIntegral sc +))
      , uWhere = (sqlInt4 (u ^. userId) .==) . view tupId
      }

updatedSolved :: User -> ProbId -> DBErr Bool
updatedSolved usr pid = tryDB runUpdate_ $ Update
  { uTable = answerTable
  , uReturning = rReturningI (view solved)
  , uUpdateWith = solved .~ sqlBool True
  , uWhere = liftA2 (.&&) ((uidField .==) . view tupUser) ((sqlInt4 (getId pid) .==) . view tupProb)
  }
  where solved :: forall s t a b. Field4 s t a b => Lens s t a b
        solved = _4
        tupProb = _1
        tupUser = _3
        uidField = sqlInt4 (usr ^. userId)
        pidField = sqlInt4 (getId pid)

markSubmission :: ProbId -> User -> Text -> DBErr Int
markSubmission pid user ans = do
  res <- verifySolution pid user ans
  if res then do
    updatedSolved user pid 
    updateScore user correctSolutionPts
  else except $ Left WrongAnswer

updateProblem :: Problem -> DBErr Problem
updateProblem p = liftQuery fromCount update uArgs
  where go = withConn (`runUpdate_` uArgs)
        setWith tLens pLens = tLens .~ sqlStrictText (p ^. pLens)
        uArgs = Update { uTable = probTable
        , uReturning = rCount
        , uUpdateWith = \(id, nm, ni, ds, sa, st) ->
            ( Just id
            , sqlStrictText (p ^. probName)
            , Just ni
            , sqlStrictText (p ^. probDesc)
            , Just sa
            , st
            )
        , uWhere = (p & (view probId >>> getId >>> sqlInt4 >>> (.==))) . view _1'
        }
        fromCount = \case
          0 -> Left ProbNotFound
          _ -> Right p

clearInputs :: ProbId -> DBErr Int
clearInputs pid = do
  let sqlPid = sqlInt4 (getId pid)
  tryQuery pure runDelete_ $
    Delete 
    { dTable = answerTable
    , dWhere = (sqlPid .==) . view _1 -- \(p, _, _, _) -> p .== sqlInt4 (getId pid)
    , dReturning = rCount
    }
  tryQuery (pure . fromIntegral) runDelete_ $ 
    Delete
    { dTable = inputTable
    , dWhere = (sqlPid .==) . view _2
    , dReturning = rCount
    }