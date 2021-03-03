{-# LANGUAGE FlexibleContexts, TypeOperators, RankNTypes #-}

module DB 
( DBErr (..)
, runDB
, runDBErr
, problemSelect 
, userSelect
, getAllProblems
, matchesId
, getProbById
, getInputsById
, addUser
, addInput
, addProblem
, getUser
, verifySolution
, updateScore
, markSubmission
) where

import Data.Int (Int64)
import Data.Profunctor.Product
import Data.Profunctor 
import Data.Tuple.Curry (Curry, uncurryN)
import Opaleye hiding (except)
import Types
import CommonModules hiding (User, userId)
import Bot.Constants
import Errors
import Utils (tShow, compareSolutions, listToEither)

type DB = ReaderT Connection IO

type DBErr = ExceptT SubmissionError DB

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

type InputFields =
  ( Field SqlInt4
  , Field SqlInt4
  , Field SqlJson
  , Field SqlText
  )

type InputTable = Table InputFields InputFields

inputTable :: InputTable
inputTable =
  table "inputs" $ p4
      ( tableField "problem_id"
      , tableField "group_id"
      , tableField "json"
      , tableField "answer"
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

problemSelect :: Select ProbFieldsO -> DB [Problem]
problemSelect ps = withConn $ (map toProblem <$>) . (`runSelect` ps)

toProblem :: (Int, Text, Int, Text, UTCTime, Text) -> Problem
toProblem = uncurryN (Problem . ProbId) . over _6 (fromMaybe NumT . toJSONType)

toUser :: (Int, Int, Text, Int, Int) -> User 
toUser = uncurryN User . (_2' %~ GroupId)

userSelect :: Select UserFieldsO -> DB [User]
userSelect us = withConn $ (map toUser <$>) . (`runSelect` us)

liftQuery :: Functor m => (a -> Either e b) -> (t -> m a) -> t -> ExceptT e m b
liftQuery modifier selector q = ExceptT $ modifier <$> selector q

firstResult :: Functor m => e -> (t -> m [a]) -> t -> ExceptT e m a
firstResult err = liftQuery (listToEither err) 

getUser :: Text -> DBErr User
getUser usr = firstResult UserNotFound userSelect q -- ExceptT $ listToEither UserNotFound <$> userSelect q
  where 
    q = do
      rows@(_, _, dsc, _, _) <- selectTable userTable
      where_ (dsc .== sqlStrictText usr)
      pure rows 


getAllProblems :: DB [Problem]
getAllProblems = problemSelect (selectTable probTable)

matchesId :: Column SqlInt4 -> Int -> Column SqlBool
matchesId a b = a .== sqlInt4 b

getProbById :: ProbId -> DBErr Problem
getProbById (ProbId p) = firstResult ProbNotFound problemSelect $ do
  rows@(_probId, _, _, _, _, _) <- selectTable probTable
  _where (_probId `matchesId` p)
  pure rows

insertVal :: Table insRow outRow -> insRow -> DB Bool
insertVal t row = (0 <) <$> withConn (`runInsert_` Insert
  { iTable = t
  , iRows = [row]
  , iReturning = rCount
  , iOnConflict = Just DoNothing
  }
  )

getInputsById :: ProbId -> GroupId -> DBErr [Inputs]
getInputsById (ProbId p) (GroupId g) = liftQuery nonEmpty sel q
  where fromRow = uncurryN Inputs . (_1' %~ pack) . (_2' %~ GroupId) . (_3' %~ pack)
        sel q = withConn $ (map fromRow <$>) . (`runSelect` q)
        nonEmpty = \case
          [] -> Left ProbNotFound
          xs -> Right xs
        q = do
          rows@(_probId, groupId, json, ans) <- selectTable inputTable
          where_ (_probId `matchesId` p)
          pure (json, groupId, ans)

-- worth adding errors here?
addUser :: Text -> GroupId -> DB Bool
addUser username (GroupId gid) = insertVal userTable 
  (Nothing, sqlInt4 gid, sqlStrictText username, 0, 0)

addProblem :: Text -> Text -> JSONType -> DB Bool
addProblem name desc ptype = insertVal probTable 
  (Nothing, sqlStrictText name, Nothing, sqlStrictText desc, Nothing, sqlStrictText $ tShow ptype)

addInput :: ProbId -> GroupId -> Text -> Text -> DB Bool
addInput pid gid inpJson ans = insertVal inputTable
  ( sqlInt4 . getId $ pid
  , sqlInt4 . getGrp $ gid
  , sqlStrictJSON . encodeUtf8 $ inpJson
  , sqlStrictText ans
  )

nthModulo :: Int -> Int -> [a] -> a
nthModulo len n = (!! (n `mod` len))

verifySolution :: ProbId -> User -> Text -> DBErr Bool
verifySolution probId user ans = do
  prob <- getProbById probId
  let gid@(GroupId grp) = user ^. userGroup
      getInput = nthModulo (prob ^. probInputs) grp
      -- this won't work if we add more inputs after a problem is released
      -- maybe have a join table relating a user's input for a problem, to the user and problem
  input <- getInput <$> getInputsById probId gid 
  pure $ compareSolutions (prob ^. probSolType) (input ^. answer) ans

-- update :: Update a -> DB 
update :: forall a. Update a -> DB a
update args = withConn (`runUpdate_` args)

updateScore :: User -> Int -> DBErr Int
updateScore u sc = firstResult DBError update uArgs
  where 
    -- these need explicit foralls in order to typecheck 
    tupId :: forall s t a b. Field1 s t a b => Lens s t a b
    tupId = _1'
    tupScore :: forall s t a b. Field4 s t a b => Lens s t a b
    tupScore  = _4'
    tupSolved = _5'
    uArgs = Update
      { uTable = userTable
      , uReturning = rReturningI (view tupScore) 
      , uUpdateWith = (tupId %~ Just) . (tupSolved %~ (+ 1)) . (tupScore %~ (fromIntegral sc +))
      , uWhere = (sqlInt4 (u ^. userId) .==) . view tupId
      }

markSubmission :: ProbId -> User -> Text -> DBErr Int 
markSubmission pid user ans = do
  res <- verifySolution pid user ans
  if res then
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


runDBErr :: MonadIO m => r -> ExceptT e (ReaderT r IO) a -> m (Either e a)
runDBErr env f = liftIO $ runReaderT (runExceptT f) env 

runDB :: MonadIO m => r -> ReaderT r IO a -> m a
runDB env f = liftIO $ runReaderT f env