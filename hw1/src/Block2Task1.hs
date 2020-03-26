{-# LANGUAGE InstanceSigs #-}

module Block2Task1 where

import Block1Task3 (TreeList(..), Tree(..))
import Data.Foldable

instance Foldable TreeList where
  toList :: TreeList a -> [a]
  toList (One a)       = [a]
  toList (Many a rest) = a : toList rest

  foldr :: (a -> b -> b) -> b -> TreeList a -> b
  foldr f b (One a)       = f a b
  foldr f b (Many a rest) = a `f` foldr f b rest

  foldMap :: Monoid m => (a -> m) -> TreeList a -> m
  foldMap f (One a)       = f a
  foldMap f (Many a rest) = f a `mappend` foldMap f rest

instance Foldable Tree where
  toList :: Tree a -> [a]
  toList Leaf                   = []
  toList (Node list left right) = toList left ++ toList list ++ toList right

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ b Leaf = b
  foldr f b tree = foldr f b (toList tree)

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f tree = foldMap f (toList tree)
