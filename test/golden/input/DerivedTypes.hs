{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DerivedTypes where

import GHC.Generics (Generic)

-- | A simple derived type using deriving stock
data Person = Person
  { personName :: String
  , personAge :: Int
  }
  deriving stock Show
  deriving stock Eq
  deriving stock Generic
  deriving anyclass ToJSON
  deriving newtype FromJSON

-- | Using regular deriving too
data Company = Company
  { companyName :: String
  , companyEmployees :: [Person]
  }
  deriving (Show, Eq)
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

-- | A sum type with record fields
data Vehicle
  = Car
      { make :: String
      , model :: String
      , year :: Int
      }
  | Motorcycle
      { make :: String
      , model :: String
      , hasSidecar :: Bool
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

-- | Combinations of deriving strategies
newtype UserId = UserId { getUserId :: Int }
  deriving newtype (Eq, Ord, Show)
  deriving stock Generic