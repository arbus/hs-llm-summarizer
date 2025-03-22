module Simple where

-- | A simple function that adds two numbers
add :: Int -> Int -> Int
add x y = x + y

-- | A simple data type
data Person = Person 
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

-- | Create a person with given name and age
createPerson :: String -> Int -> Person
createPerson n a = Person n a