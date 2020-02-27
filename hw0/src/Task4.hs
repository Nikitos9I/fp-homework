module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

-- | Generate infinite loop from element `a`
iterateElement :: a -> [a]
iterateElement element = fix (\accum -> element : accum)

-- | Getting a Fibonacci number with the desired number
fibonacci :: Integer -> Integer
fibonacci =
  fix
    (\accum n ->
       case n <= 2 of
         True  -> min n 1
         False -> accum (n - 1) + accum (n - 2))

-- | Getting a factorial for a number
factorial :: Integer -> Integer
factorial =
  fix
    (\accum n ->
       case n <= 1 of
         True  -> 1
         False -> n * accum (n - 1))

-- | Getting new list with applied function @f to source list
mapFix :: (a -> b) -> [a] -> [b]
mapFix f =
  fix
    (\accum (x : xs) ->
       case null xs of
         True  -> [f x]
         False -> (f x) : accum xs)
