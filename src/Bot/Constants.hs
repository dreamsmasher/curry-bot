module Bot.Constants where

import CommonModules
import Language.Haskell.TH.Syntax qualified as TH 
correctSolutionPts :: Int
correctSolutionPts = 100

botName :: Text
botName = "currybot"

-- is this worth making static? in case others fork or we migrate this to an org
botRepoUrl :: Text
botRepoUrl = "https://github.com/dreamsmasher/curry-bot"

-- 24 kB, subject to change
-- oversized input error string is automatically created at compile time
maxSubmissionSize :: Integer
maxSubmissionSize = 24000 