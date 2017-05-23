
module DB.StateUser
  ( UserInfo(..) )
  where

import qualified Data as D

-- | Lives in a seperate module due to record name conflicts... Sad!

data UserInfo = UserInfo
  { userId    :: D.UserID
  , userEmail :: String
  , userPass  :: D.Password
  } deriving Show
