module Task6
  ( task6Function1
  , task6Function2
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing

-- | function for packing a value Left from char list into a pair 
task6Function1 :: (Either String b, Either String c)
-- (Either String b, Either String c) - whnf, 
-- because (,) - outermost pair constructor
-- Result whnf: 
--    ( Left ("harold" ++ " hide " ++ "the " ++ "pain"), 
--    , Left ("harold" ++ " hide " ++ "the " ++ "pain")
--    )
task6Function1 =
  distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | function for checking the list obtained by converting the letters 'o' 
-- from the list of characters to the exponent of the number PI, to empty
task6Function2 :: Bool
-- Bool - whnf
-- Result whnf: (False)
task6Function2 = null $ mapMaybe foo "pole chudes ochen' chudesno"
