module TypeOperatorTest where

-- The problematic import with explicit type operator
import Servant (CaptureAll, type (:<|>) (..))

-- A function using the type operator
combined :: a -> b -> (a :<|> b)
combined a b = undefined