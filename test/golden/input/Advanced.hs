{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Advanced where

import Data.Kind (Type)
import qualified Data.Text as T

-- | Phantom type for database tables
data Table a = Table String

-- | Phantom type for table column
data Column a b = Column String

-- | Class for manipulating database tables
class Database db where
  -- | Type of query result
  type QueryResult db :: Type

  -- | Execute a query
  query :: db -> String -> IO (QueryResult db)
  
  -- | Create a new table
  createTable :: Table a -> IO ()
  
  -- | Drop a table
  dropTable :: Table a -> IO ()

-- | Class for orm relationships
class (Database db) => Entity db e | e -> db where
  -- | Get database table for entity
  tableName :: e -> String
  
  -- | List of column names
  columns :: e -> [String]
  
  -- | Primary key
  primaryKey :: e -> String
  
  -- | Get all entities
  getAll :: IO [e]
  
  -- | Get entity by id
  getById :: Int -> IO (Maybe e)
  
  -- | Save entity
  save :: e -> IO e

-- | MySQL database implementation
data MySQL = MySQL { connectionString :: String }

-- | Instance for MySQL database
instance Database MySQL where
  type QueryResult MySQL = [[String]]
  
  query db q = do
    putStrLn $ "Executing on " ++ connectionString db ++ ": " ++ q
    return []
  
  createTable (Table name) = do
    putStrLn $ "Creating table " ++ name
    return ()
  
  dropTable (Table name) = do
    putStrLn $ "Dropping table " ++ name
    return ()

-- | User entity
data User = User
  { userId :: Int
  , userName :: T.Text
  , userEmail :: T.Text
  }

-- | Instance for User entity
instance Entity MySQL User where
  tableName _ = "users"
  
  columns _ = ["id", "name", "email"]
  
  primaryKey _ = "id"
  
  getAll = error "Not implemented"
  
  getById id = do
    putStrLn $ "Getting user with id: " ++ show id
    return Nothing
  
  save user = do
    putStrLn $ "Saving user: " ++ T.unpack (userName user)
    return user