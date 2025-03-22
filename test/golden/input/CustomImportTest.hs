module CustomImportTest where

import PI.Db.User qualified as Db

-- | User data type that wraps around the database user
data User = User
  { userId   :: Int
  , userName :: String
  , userAge  :: Int
  }

-- | Convert database user to application user
fromDbUser :: Db.User -> User
fromDbUser dbUser = User
  { userId   = Db.id dbUser
  , userName = Db.name dbUser
  , userAge  = Db.age dbUser
  }

-- | Create a new user
createUser :: String -> Int -> User
createUser name age = User
  { userId   = 0  -- Will be assigned by database
  , userName = name
  , userAge  = age
  }