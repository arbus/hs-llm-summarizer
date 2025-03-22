{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeApplicationTest where

import Database.Query (DbrOnly)
import Data.Kind (Type)

-- Example of TypeApplications and DataKinds together
data BackendType = NB | EB

-- Function using TypeApplications with DataKinds
querySmallList :: forall (b :: BackendType) (r :: Type) . Int -> IO [r]
querySmallList n = undefined  -- Simplified to parse more easily

-- More complex example
class MultiBackend (b :: BackendType) where
  runMultiQuery :: forall (r :: Type) . Int -> IO [r]

instance MultiBackend 'NB where
  runMultiQuery = undefined