{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module DB 
( problemSelect
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

import Control.Lens
import Control.Arrow
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except 
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.Trans.Maybe
import Errors
import Control.Monad.Trans.Reader
import Data.Bool (bool)
import Data.Int (Int64)
import Data.Functor
import Data.Maybe
import Data.Profunctor.Product
import Data.Profunctor 
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Tuple.Curry (Curry, uncurryN)
import Database.PostgreSQL.Simple (Connection)
import Opaleye hiding (except)
import Types
import Constants
import Utils (tShow, compareSolutions, listToEither)

type DB = ReaderT Connection IO

type DBErr = ExceptT SubmissionError DB

type ContrJoin a = Table a a

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
  where toProbType = undefined

toProblem :: (Int, Text, Int, Text, UTCTime, Text) -> Problem
toProblem = uncurryN (Problem . ProbId) . over _6 (fromMaybe NumT . toJSONType)

toUser :: (Int, Int, Text, Int, Int) -> User 
toUser = uncurryN User . (_2 %~ GroupId)

userSelect :: Select UserFieldsO -> DB [User]
userSelect us = withConn $ (map toUser <$>) . (`runSelect` us)

getUser :: Text -> DB (SubmissionResult User)
getUser usr = listToEither UserNotFound  <$> userSelect ( do
  rows@(_, _, dsc, _, _) <- selectTable userTable
  where_ (dsc .== sqlStrictText usr)
  pure rows 
  )

getAllProblems :: DB [Problem]
getAllProblems = problemSelect (selectTable probTable)

matchesId :: Column SqlInt4 -> Int -> Column SqlBool
matchesId a b = a .== sqlInt4 b

getProbById :: ProbId -> DB (Maybe Problem)
getProbById (ProbId p) = listToMaybe <$> problemSelect ( do
  rows@(_probId, _, _, _, _, _) <- selectTable probTable
  _where (_probId `matchesId` p)
  pure rows
  )

insertVal :: Table insRow outRow -> insRow -> DB Bool
insertVal t row = (0 <) <$> withConn (`runInsert_` Insert
  { iTable = t
  , iRows = [row]
  , iReturning = rCount
  , iOnConflict = Just DoNothing
  }
  )

getInputsById :: ProbId -> GroupId -> DB [Inputs]
getInputsById (ProbId p) (GroupId g) = withConn ((map fromRow <$>) . (`runSelect` q))
  where fromRow = uncurryN Inputs . (_1 %~ pack) . (_2 %~ GroupId) . (_3 %~ pack)
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
verifySolution probId user ans = lift ask >>= \conn -> do
  prob <- maybeToExceptT ProbNotFound . MaybeT $ getProbById probId
  let gid@(GroupId grp) = user ^. userGroup
      getInput = nthModulo (prob ^. probInputs) grp
      -- this won't work if we add more inputs after a problem is released
      -- maybe have a join table relating a user's input for a problem, to the user and problem
  input <- getInput <$> lift (getInputsById probId gid) 
  pure $ compareSolutions (prob ^. probSolType) (input ^. answer) ans

updateScore :: User -> Int -> DBErr Int
updateScore u sc = ExceptT (listToEither DBError <$> withConn (`runUpdate_` uArgs))
  where tupId     = _1 -- these have different types from the context
        tupId'    = _1 -- TODO existentially quantify these lenses to avoid repeats
        tupScore  = _4
        tupScore' = _4
        tupSolved = _5 -- tuples everywhere....
        uArgs = Update
          { uTable = userTable
          , uReturning = rReturningI (view tupScore') 
          , uUpdateWith = (tupId %~ Just) . (tupSolved %~ (+ 1)) . (tupScore %~ (fromIntegral sc +))
          , uWhere = (sqlInt4 (u ^. userId) .==) . view tupId'
          }

markSubmission :: ProbId -> User -> Text -> DBErr Int 
markSubmission pid user ans = do
  res <- verifySolution pid user ans
  if res then
    updateScore user correctSolutionPts 
  else except $ Left WrongAnswer

updateProblem :: Problem -> DBErr Problem
updateProblem p = ExceptT (fromCount <$> withConn (`runUpdate_` uArgs))
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
        , uWhere = (p & (view probId >>> getId >>> sqlInt4 >>> (.==))) . view _1
        }
        fromCount = \case
          0 -> Left ProbNotFound
          _ -> Right p