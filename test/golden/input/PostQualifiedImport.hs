module PostQualifiedImport where

-- New style import syntax (post-qualified imports) introduced in GHC 8.10
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Data.Set qualified

-- | A simple record using qualified imports
data Person = Person
  { name :: T.Text
  , age :: Int
  , friends :: Map.Map T.Text Int
  , interests :: Data.Set.Set T.Text
  }

-- | Create a new person
createPerson :: T.Text -> Int -> Person
createPerson name age = Person
  { name = name
  , age = age
  , friends = Map.empty
  , interests = Data.Set.empty
  }

-- | Add a friend with a score
addFriend :: Person -> T.Text -> Int -> Person
addFriend person friendName score = person
  { friends = Map.insert friendName score (friends person)
  }

-- | Add an interest
addInterest :: Person -> T.Text -> Person
addInterest person interest = person
  { interests = Data.Set.insert interest (interests person)
  }