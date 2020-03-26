module Block4Task1
  ( stringSum
  ) where

import Block2Task2
import Data.Char hiding (isNumber)
import Data.List.NonEmpty hiding (dropWhile, filter)

isNumber :: String -> Bool
isNumber "." = False
isNumber "" = False
isNumber xs =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

-- | Get sum of numbers in string safety
stringSum :: String -> Maybe Float
stringSum st =
  traverse
    (\x ->
       if isNumber x
         then Just (read x :: Float)
         else Nothing)
    (filter (not . null) (toList $ splitOn ' ' st)) >>=
  Just . sum
