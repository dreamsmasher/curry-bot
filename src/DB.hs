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
) where

import Control.Lens
import Control.Arrow
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import Errors
import Control.Monad.Trans.Reader
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
import Opaleye
import Types
import Control.Monad.Trans.Except ( ExceptT )

type DB = ReaderT Connection IO

type DBErr = ExceptT SubmissionError DB

type ContrJoin a = Table a a

type ProbFieldsI =
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Maybe (Field SqlInt4)
  , Field SqlText
  , Maybe (Field SqlTimestamptz)
  )

type ProbFieldsO =
  ( Field SqlInt4
  , Field SqlText
  , Field SqlInt4
  , Field SqlText
  , Field SqlTimestamptz
  )

type ProbTable = Table ProbFieldsI ProbFieldsO

serialField :: String -> TableFields (Maybe (Column a)) (Column a)
serialField = optionalTableField

probTable :: ProbTable
probTable =
  table "problems" $ p5
      ( serialField "id"
      , tableField  "name"
      , serialField "n_inputs"
      , tableField  "description"
      , serialField "submitted_at"
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
      , tableField  "discord_name"
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
problemSelect ps = withConn $ (map (uncurryN $ Problem . ProbId) <$>) . (`runSelect` ps)

toUser :: (Int, Int, Text, Int, Int) -> User 
toUser = uncurryN User . (_2 %~ GroupId)

userSelect :: Select UserFieldsO -> DB [User]
userSelect us = withConn $ (map toUser <$>) . (`runSelect` us)

getUser :: Text -> DB (Maybe User)
getUser usr = listToMaybe <$> userSelect ( do
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
  rows@(_probId, _, _, _, _) <- selectTable probTable
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

addUser :: Text -> GroupId -> DB Bool
addUser username (GroupId gid) = insertVal userTable 
  (Nothing, sqlInt4 gid, sqlStrictText username, 0, 0)

addProblem :: Text -> Text -> DB Bool
addProblem name desc = insertVal probTable 
  (Nothing, sqlStrictText name, Nothing, sqlStrictText desc, Nothing)

addInput :: ProbId -> GroupId -> Text -> Text -> DB Bool
addInput pid gid inpJson ans = insertVal inputTable
  ( sqlInt4 . getId $ pid
  , sqlInt4 . getGrp $ gid
  , sqlStrictJSON . encodeUtf8 $ inpJson
  , sqlStrictText ans
  )

nthModulo :: Int -> Int -> [a] -> a
nthModulo len n = (!! (n `mod` len))

verifySolution :: ProbId -> User -> Text -> DB Bool
verifySolution probId user ans = do
  maybeProb <- getProbById probId
  flip (maybe (pure False)) maybeProb $ \prb -> do
    let grp = user ^. userGroup;
    inp <- nthModulo (prb ^. probInputs) (getGrp grp) <$> getInputsById probId (user ^. userGroup)
    pure $ inp ^. answer == ans

updateScore :: User -> Int -> DB (Maybe Int)
updateScore u sc = listToMaybe <$> withConn (`runUpdate_` Update 
  { uTable = userTable
  , uReturning = rReturningI (view _4) 
    -- different type from the update fields
    -- and existentially quantifying tupScore with a forall would
    -- require more imports
  , uUpdateWith = (tupId %~ Just) . (tupSolved %~ (+ 1)) . (tupScore %~ (fromIntegral sc +))
  , uWhere = (sqlInt4 (u ^. userId) .==) . view _1
  })
  where tupId     = _1
        tupScore  = _4
        tupSolved = _5 -- tuples everywhere....

updateProblem :: Problem -> DB (Maybe Problem)
updateProblem p = Just p <$ withConn (`runUpdate_` Update 
  { uTable = probTable
  , uReturning = rCount
  , uUpdateWith = \(id, nm, ni, ds, sa) -> 
      ( Just id
      , sqlStrictText (p ^. probName)
      , Just ni
      , sqlStrictText (p ^. probDesc)
      , Just sa
      )
  , uWhere = (p & (view probId >>> getId >>> sqlInt4 >>> (.==))) . view _1
  })
  where setWith tLens pLens = tLens .~ sqlStrictText (p ^. pLens)
