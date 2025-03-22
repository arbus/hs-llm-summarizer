module NewImportSyntax where

-- Old style import syntax for compatibility with haskell-src-exts
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set

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