module Task2
  ( doubleNeg
  , excludedNeg
  , doubleNegElim
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | Double negative
doubleNeg :: a -> Neg (Neg a)
doubleNeg a notA = notA a

axiom9 :: (a -> b) -> (a -> Neg b) -> Neg a
axiom9 g f x = f x $ g x

contrPosition :: (a -> b) -> (Neg b -> Neg a)
contrPosition g f x = f $ g x

-- | Rule of the excluded third
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = axiom9 (contrPosition Left) (contrPosition Right)

-- | Pearce's law cannot be proved in intuitionistic logic
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | Removing the double negatives cannot be proved in intuitionistic logic
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | Removing the third negatives
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f a = f $ doubleNeg a
