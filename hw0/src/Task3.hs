module Task3
  ( composition
  , identity
  , contraction
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | Analog for the function (.)
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s . const

-- | Analog for the function (id)
identity :: a -> a
identity = s const const

-- | Function for applying the first argument to the second argument twice
contraction :: (a -> a -> b) -> a -> b
contraction = s s $ const identity

-- | Function for applying the first argument to the third argument firstly
-- and to the second argument secondly
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s ((s $ const s) ((s $ const const) s)) $ const const
