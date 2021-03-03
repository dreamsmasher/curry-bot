module Bot.Constants where

import CommonModules
correctSolutionPts :: Int
correctSolutionPts = 100

botName :: Text
botName = "currybot"

-- is this worth making static? in case others fork or we migrate this to an org
botRepoUrl :: Text
botRepoUrl = "https://github.com/dreamsmasher/curry-bot"