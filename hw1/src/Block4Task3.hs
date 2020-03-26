module Block4Task3 where

import Control.Applicative

data NonEmpty a =
  a :| [a]
  deriving (Show)

(<|) :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x :| xs) <| (y :| ys) = x :| (xs ++ (y : ys))

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []
  f :| [] <*> xs = f <$> xs
  f :| (hd:tl) <*> xs = (f <$> xs) <| (hd :| tl <*> xs)

instance Foldable NonEmpty where
  foldr f z (x :| xs) = f x (foldr f z xs)
  foldMap f (x :| xs) = f x <> foldMap f xs

instance Traversable NonEmpty where
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)
