{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Complex where

import Data.Kind (Type)

-- | A GADT for representing expressions
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Equal :: Expr Int -> Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

-- | Type family for determining result type
type family ResultType a where
  ResultType Int = Int
  ResultType Bool = Bool
  ResultType (a -> b) = ResultType b

-- | Evaluate an expression
eval :: Expr a -> a
eval (LitInt i) = i
eval (LitBool b) = b
eval (Add x y) = eval x + eval y
eval (Equal x y) = eval x == eval y
eval (If cond then' else') = if eval cond then eval then' else eval else'

-- | High-order function that applies a function to an expression
applyFn :: (a -> b) -> Expr a -> Expr b
applyFn f expr = undefined -- Implementation left as an exercise

-- | A class for converting values to expressions
class ToExpr a where
  toExpr :: a -> Expr a

-- | Instance for Int
instance ToExpr Int where
  toExpr = LitInt

-- | Instance for Bool
instance ToExpr Bool where
  toExpr = LitBool