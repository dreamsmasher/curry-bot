{-# LANGUAGE FlexibleContexts #-}

module DB where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Functor
import Data.Profunctor.Product
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Tuple.Curry (Curry, uncurryN)
import Database.PostgreSQL.Simple (Connection)
import Opaleye
import Types

type ContrJoin a = Table a a

type ProbFieldsI =
  ( Maybe (Field SqlInt4)
  , Field SqlText
  , Field SqlText
  , Field SqlTimestamptz
  )

type ProbFieldsO =
  ( Field SqlInt4
  , Field SqlText
  , Field SqlText
  , Field SqlTimestamptz
  )

type ProbTable = Table ProbFieldsI ProbFieldsO

ccTable :: String -> TableFields writeFields viewFields -> Table writeFields viewFields
ccTable = tableWithSchema "CCData"

serialField :: String -> TableFields (Maybe (Column a)) (Column a)
serialField = optionalTableField

probTable :: ProbTable
probTable =
  ccTable "Problems" $ p4
      ( serialField "id"
      , tableField  "name"
      , tableField  "description"
      , tableField  "submitted_at"
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
  ccTable "Users" $ p5
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
  , Field SqlInt4
  )

type InputTable = Table InputFields InputFields

inputTable :: InputTable
inputTable =
  ccTable "Inputs" $ p4
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

userSelect :: Select UserFieldsO -> DB [User]
userSelect us = withConn $ (map (uncurryN User . (_2 %~ GroupId)) <$>) . (`runSelect` us)

getAllProblems :: DB [Problem]
getAllProblems = problemSelect (selectTable probTable)

matchId :: Column SqlInt4 -> ProbId -> Select ()
matchId sqlId probId = _where $ sqlId .== sqlInt4 (getId probId)

getProbById :: ProbId -> DB [Problem]
getProbById probId = problemSelect $ do
  rows@(_probId, _, _, _) <- selectTable probTable
  matchId _probId probId
  pure rows

getInputsById :: ProbId -> DB [Inputs]
getInputsById probId = withConn $ (map fromRow <$>) . (`runSelect` q)
  where fromRow = uncurryN Inputs . (_1 %~ pack) . (_2 %~ GroupId)
        q = do
          rows@(_probId, groupId, json, ans) <- selectTable inputTable
          matchId _probId probId
          pure (json, groupId, ans)

addUser :: Text -> GroupId -> DB User
addUser username (GroupId gid) = withConn $ flip runInsert_ Insert
  { iTable = userTable
  , iRows = userRow
  , iReturning = undefined
  , iOnConflict = Just DoNothing
  }
  where userRow = [(Nothing, sqlInt4 gid, sqlStrictText username, 0, 0)]
