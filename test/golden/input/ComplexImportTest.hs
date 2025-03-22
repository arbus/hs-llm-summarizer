module ComplexImportTest where

-- Example with multiple imports, some with selectors
import Data.Text qualified as T
import Data.Map.Strict qualified as Map (empty, insert, lookup)
import Control.Monad.Reader qualified
import qualified Network.HTTP.Client as HTTP
import Data.Vector qualified
import Data.Aeson (ToJSON, FromJSON, (.=))
import qualified Data.ByteString.Lazy
import GHC.Generics (Generic)
import "base" Data.Maybe (fromMaybe)

-- | A complex record using many qualified imports
data ComplexData = ComplexData
  { text :: T.Text
  , mapping :: Map.Map T.Text Int
  , httpClient :: HTTP.Manager
  , vector :: Data.Vector.Vector Int
  , binary :: Data.ByteString.Lazy.ByteString
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Create an empty complex data structure
emptyComplex :: HTTP.Manager -> ComplexData
emptyComplex mgr = ComplexData
  { text = T.empty
  , mapping = Map.empty
  , httpClient = mgr
  , vector = Data.Vector.empty
  , binary = Data.ByteString.Lazy.empty
  }