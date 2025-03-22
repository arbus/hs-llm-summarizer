{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RecordPatterns where

-- | Person record
data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  , address :: Address
  }

-- | Address record
data Address = Address
  { street :: String
  , city :: String
  , country :: String
  , postalCode :: String
  }

-- | Using NamedFieldPuns for record pattern matching
greetPerson :: Person -> String
greetPerson Person{firstName, lastName} =
  "Hello, " ++ firstName ++ " " ++ lastName ++ "!"

-- | Accessing a nested field with NamedFieldPuns
getCity :: Person -> String
getCity Person{address = Address{city}} = city

-- | Using RecordWildCards to grab all fields
createSummary :: Person -> String
createSummary Person{..} =
  firstName ++ " " ++ lastName ++ " is " ++ show age ++ 
  " years old and lives in " ++ city ++ ", " ++ country

-- | Combining wildcards and specific fields
getAddressLabel :: Person -> String
getAddressLabel Person{firstName, lastName, address = Address{..}} =
  firstName ++ " " ++ lastName ++ "\n" ++
  street ++ "\n" ++
  city ++ ", " ++ postalCode ++ "\n" ++
  country

-- | Using wildcards with let bindings
incrementAge :: Person -> Person
incrementAge p@Person{..} =
  p{age = age + 1}

-- | Nested wildcards
moveToNewCity :: Person -> String -> Person
moveToNewCity p@Person{address = a@Address{..}} newCity =
  p{address = a{city = newCity}}