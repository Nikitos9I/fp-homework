module Block2Task2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty
import Data.Maybe

-- | Split a string with separators
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn el lstIn =
  fromJust $
  nonEmpty $
  uncurry
    (:)
    (foldr
       (\x (lst, ans) ->
          if x /= el
            then (x : lst, ans)
            else ([], lst : ans))
       ([], [])
       lstIn)

-- | Join a string with separators
joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith el = Prelude.init . foldr (\x lst -> x ++ el : lst) [] . toList
