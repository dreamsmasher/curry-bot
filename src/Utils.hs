module Utils where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe
import Text.Read (readMaybe)
import Discord.Internal.Types.User qualified as U
import Types

-- TODO make this less garbage
genUserGroup :: U.User -> GroupId
genUserGroup user = GroupId 
                  $ fromMaybe 0 (readMaybe (T.unpack (U.userDiscrim user))) `mod` 20
                    