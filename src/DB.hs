{-# LANGUAGE FlexibleContexts #-}

module DB where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
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

type ProbFields =
  ( Field SqlInt4,
    Field SqlText,
    Field SqlText,
    Field SqlTimestamptz
  )

type ProbTable = ContrJoin ProbFields

ccTable :: String -> TableFields writeFields viewFields -> Table writeFields viewFields
ccTable = tableWithSchema "CCData"

probTable :: ProbTable
probTable =
  ccTable "Problems" $
    p4
      ( tableField "id",
        tableField "name",
        tableField "description",
        tableField "submitted_at"
      )

type UserFields =
  ( Field SqlInt4,
    Field SqlText,
    Field SqlInt4,
    Field SqlInt4
  )

type UserTable = ContrJoin UserFields

userTable :: UserTable
userTable =
  ccTable "Users" $
    p4
      ( tableField "id",
        tableField "discord_name",
        tableField "score",
        tableField "solved"
      )

type InputFields =
  ( Field SqlInt4,
    Field SqlJson,
    Field SqlInt4
  )

type InputTable = ContrJoin InputFields

inputTable :: InputTable
inputTable =
  ccTable "Inputs" $
    p3
      ( tableField "problem_id",
        tableField "json",
        tableField "answer"
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

problemSelect :: Select ProbFields -> DB [Problem]
problemSelect ps = withConn $ (map (uncurryN Problem . over _1 ProbId) <$>) . (`runSelect` ps)

userSelect :: Select UserFields -> DB [User]
userSelect us = withConn $ (map (uncurryN User) <$>) . (`runSelect` us)

getAllProblems :: DB [Problem]
getAllProblems = problemSelect (selectTable probTable)

matchId :: Column SqlInt4 -> ProbId -> Select ()
matchId sqlId probId = _where (sqlId .== sqlInt4 (getId probId))

getProbById :: ProbId -> DB [Problem]
getProbById probId = problemSelect $ do
  rows@(_probId, _, _, _) <- selectTable probTable
  matchId _probId probId
  pure rows

getInputsById :: ProbId -> DB [Inputs]
getInputsById probId = withConn $ (map (uncurry $ Inputs . pack) <$>) . (`runSelect` q)
  where q = do
          rows@(_probId, json, ans) <- selectTable inputTable
          matchId _probId probId
          pure (json, ans)
