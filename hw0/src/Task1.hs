{-# LANGUAGE TypeOperators #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

type (<->) a b = (a -> b, b -> a)

-- | Distributivity low for Either
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a)  = (Left a, Left a)
distributivity (Right x) = (Right $ fst x, Right $ snd x)

-- | Associativity low for pair of pair
associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

eitherLeftHelper :: Either a (Either b c) -> Either (Either a b) c
eitherLeftHelper (Left x)          = Left . Left $ x
eitherLeftHelper (Right (Left x))  = Left . Right $ x
eitherLeftHelper (Right (Right x)) = Right x

eitherRightHelper :: Either (Either a b) c -> Either a (Either b c)
eitherRightHelper (Left (Left x))  = Left x
eitherRightHelper (Left (Right x)) = Right . Left $ x
eitherRightHelper (Right x)        = Right . Right $ x

-- | Associativity low for Either of Either
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (eitherLeftHelper, eitherRightHelper)
