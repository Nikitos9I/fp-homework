module Block3Task1
  ( maybeConcat
  , eitherConcat
  ) where

import Data.Either
import Data.Maybe

-- | Concat elements of Maybe list to list
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat xs = mconcat $ map (fromMaybe []) xs

-- | Concat elements of Either list to pair
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat xs = (mconcat $ lefts xs, mconcat $ rights xs)
