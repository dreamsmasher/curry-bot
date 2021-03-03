module Bot.Embed where

import Discord
import Discord.Internal.Types

import Types
import CommonModules
import Bot.Constants
import Utils

-- TODO figure out a way to embed longer problesm 
-- embeds have a limit of 2048 characters for the description
-- maybe split it up into chunks? 
-- limit is 2048 chars desc, 25 * (1024 chars) fields, 2048 chars footer
embedProblem :: Problem -> CreateEmbed
embedProblem p = def 
  { createEmbedAuthorName = botName
  , createEmbedAuthorUrl = botRepoUrl
  , createEmbedThumbnail = Nothing -- TODO make thumbnail
  , createEmbedUrl = botRepoUrl
  , createEmbedTitle = pack $ liftA2 (printf "%d: %s") (getId . view probId) (view probName) p
  , createEmbedDescription = p ^. probDesc
  , createEmbedFields = map (uncurry fmtFields . fmap ($ p)) 
    [ ("Solution type", tShow . view probSolType)
    , ("Problem ID", tShow . getId . view probId)
    , ("Submitted at", tShow . utctDay . view probSubmit)
    ]
  }
  where fmtFields title body = EmbedField title body (Just True)

testProb :: Problem
testProb = Problem (ProbId 10) "Jaden Case" 50 "Given an input string, turn it into a Jaden Smith tweet: capitalize every word of the string. Submit your answer as a string." t StrT
  where t = read "2021-03-03 03:31:27.486597366 UTC"