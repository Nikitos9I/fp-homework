module Block1Task2
  ( Nat(..)
  , sumNat
  , subNat
  , mulNat
  , convertNatToInt
  , convertIntToNat
  , equalNat
  , compareNat
  , parityNat
  ) where

data Nat
  = Z
  | S Nat
  deriving (Show)

-- | Sum of two Nats
sumNat :: Nat -> Nat -> Nat
sumNat a (S b) = sumNat (S a) b
sumNat a _     = a

-- | Substitution of two Nats
subNat :: Nat -> Nat -> Nat
subNat (S a) (S b) = subNat a b
subNat Z (S _)     = error "The first natural number must be grather then second"
subNat a Z         = a

mulNat' :: Nat -> Nat -> Nat -> Nat
mulNat' _ res Z = res
mulNat' a an bn = mulNat' a (sumNat a an) (subNat bn $ S Z)

-- | Multiplication of two Nats
mulNat :: Nat -> Nat -> Nat
mulNat Z _     = Z
mulNat _ Z     = Z
mulNat a (S b) = mulNat' a a b

-- | Convert Int to Nat
convertIntToNat :: Int -> Nat
convertIntToNat 0 = Z
convertIntToNat n =
  if n < 0
    then error "Incorrect convertation"
    else S $ convertIntToNat $ n - 1

-- | Convert Nat to Int
convertNatToInt :: Nat -> Int
convertNatToInt Z     = 0
convertNatToInt (S a) = 1 + convertNatToInt a

-- | Check Nats equality
equalNat :: Nat -> Nat -> Bool
equalNat (S a) (S b) = equalNat a b
equalNat Z Z         = True
equalNat _ _         = False

-- | Comparing two Nats
compareNat :: Nat -> Nat -> Int
compareNat (S a) (S b) = compareNat a b
compareNat (S _) Z     = 1
compareNat Z Z         = 0
compareNat Z (S _)     = -1

-- | Parity of Nat
parityNat :: Nat -> Bool
parityNat a = convertNatToInt a `mod` 2 == 0
