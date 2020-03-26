{-# LANGUAGE InstanceSigs #-}

module Block3Task2
  ( NonEmpty(..)
  , ThisOrThat(..)
  , Name(..)
  , Endo(..)
  ) where

data NonEmpty a =
  a :| [a]
  deriving (Show, Eq)

instance Semigroup a => Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (l1 :| l1r) (l2 :| l2r) = (l1 <> l2) :| (l1r <> l2r)

data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (This a) (That b)     = Both a b
  (<>) (This a) (This b)     = This (a <> b)
  (<>) (This a) (Both b c)   = Both (a <> b) c
  (<>) (That a) (This b)     = Both b a
  (<>) (That a) (That b)     = That (a <> b)
  (<>) (That a) (Both b c)   = Both b (a <> c)
  (<>) (Both a b) (This c)   = Both (a <> c) b
  (<>) (Both a b) (That c)   = Both a (b <> c)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

newtype Name =
  Name String
  deriving (Show, Eq)

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) (Name a) (Name b) = Name (a ++ '.' : b)

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

newtype Endo a =
  Endo
    { getEndo :: a -> a
    }

instance (Semigroup a) => Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) (Endo a) (Endo b) = Endo (a <> b)

instance (Monoid a) => Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id
