module Block5Task1
  ( Expr(..)
  , ArithmeticError(..)
  , eval
  ) where

import Control.Monad

-- | Expression
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Eq)

data ArithmeticError
  = DivisionByZero
  | PowMinus
  deriving (Eq)

instance Show ArithmeticError where
  show DivisionByZero = "Division by zero"
  show PowMinus       = "Exponentiation with negate power"

eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either ArithmeticError Int
eval' f a b = liftM2 f (eval a) (eval b)

-- | Compute expression
eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval (Add a b) = eval' (+) a b
eval (Sub a b) = eval' (-) a b
eval (Mul a b) = eval' (*) a b
eval (Div a b) =
  liftM2 div (eval a) $
  eval b >>=
  (\x ->
     if x == 0
       then Left DivisionByZero
       else Right x)
eval (Pow a b) =
  liftM2 (^) (eval a) $
  eval b >>=
  (\x ->
     if x < 0
       then Left PowMinus
       else Right x)
