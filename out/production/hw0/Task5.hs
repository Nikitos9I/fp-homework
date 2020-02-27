module Task5
  ( zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

-- | Zero Church numeral
zero :: Nat a
zero f x = x

-- | Increment Church numeral
succChurch :: Nat a -> Nat a
succChurch f = (.) <*> f

-- | Sum of two Church numerals
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus lnum rnum z = (.) (lnum z) (rnum z)

-- | Composition of two Church numerals
churchMult :: Nat a -> Nat a -> Nat a
churchMult lnum rnum = lnum . rnum

-- | Conversion of Church numeral to Integer
churchToInt :: Nat Integer -> Integer
churchToInt num = num (+ 1) 0
