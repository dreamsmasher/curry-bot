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
embedProblem p@Problem 
  { _probSolType = jsType
  , _probId      = pid
  , _probSubmit  = time
  , _probName    = name
  } = def 
    { createEmbedAuthorName  = botName
    , createEmbedAuthorUrl   = botRepoUrl
    , createEmbedThumbnail   = Nothing -- TODO make thumbnail
    , createEmbedUrl         = botRepoUrl -- TODO have this link to the problem somehow?
    , createEmbedTitle       = pack $ printf "%d: %s" (getId pid) name
    , createEmbedDescription = p ^. probDesc
    , createEmbedFields      = map fmtFields
      [ ("Solution type", tShow jsType)
      , ("Problem ID", tShow $ getId pid)
      , ("Submitted at", tShow $ utctDay time)
      ]
    }
  where fmtFields (title, body) = EmbedField title body (Just True)