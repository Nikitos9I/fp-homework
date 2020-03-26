module Block4Task2 where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf
  (<*>) f (Leaf x)     = fmap ($ x) f
  (<*>) f (Branch l r) = Branch (f <*> l) (f <*> r)

instance Foldable Tree where
  foldMap f (Leaf x)     = f x
  foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r

instance Traversable Tree where
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r
