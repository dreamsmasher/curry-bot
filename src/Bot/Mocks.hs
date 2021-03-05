module Bot.Mocks where
import Errors
import Data.Aeson (Value)
import Discord.Internal.Types.User as U
import Discord

import CommonModules
import Utils

-- TODO delete these tests before deploy
dummyMsg :: Text -> Maybe Attachment -> Message
dummyMsg t a = Message 0 0 dummyUsr t tme Nothing False False [] [] (maybeToList a) [] [] Nothing False Nothing
  where tme = read "2021-03-04 00:45:41.091531966 UTC"

dummyUsr :: User
dummyUsr = U.User 0 "" "" Nothing False False Nothing Nothing Nothing

dmt :: Message
dmt = dummyMsg "" . Just $ Attachment 0 "file.txt" 20048 "http://api.nliu.net/rants" "" Nothing Nothing