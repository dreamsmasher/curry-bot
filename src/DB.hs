module DB where

import Types
import Data.Profunctor.Product
import Opaleye
import Data.Time (UTCTime)

type ContrJoin a = Table a a

type ProbFields = 
    ( Field SqlInt4
    , Field SqlText
    , Field SqlText
    , Field SqlText
    )

type ProbTable = ContrJoin ProbFields

ccTable :: String -> TableFields writeFields viewFields -> Table writeFields viewFields
ccTable = tableWithSchema "CCData"

probTable :: ProbTable
probTable = ccTable "Problems" $ p4 
    ( tableField "id"
    , tableField "name"
    , tableField "description"
    , tableField "submitted_at"
    )

type UserFields = 
    ( Field SqlInt4
    , Field SqlText
    , Field SqlInt4
    , Field SqlInt4
    )

type UserTable = ContrJoin UserFields

userTable :: UserTable
userTable = ccTable "Users" $ p4 
    ( tableField "id"
    , tableField "discord_name"
    , tableField "score"
    , tableField "solved"
    )

type InputFields = 
    ( Field SqlInt4
    , Field SqlJson
    , Field SqlInt4
    )

type InputTable = ContrJoin InputFields

inputTable :: InputTable
inputTable = ccTable "Inputs" $ p3 
    ( tableField "problem_id"
    , tableField "json"
    , tableField "answer"
    )

