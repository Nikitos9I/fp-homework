module Task7
  ( task7Function1
  , task7Function2
  , task7Function3
  ) where

import Data.Either (lefts, rights)

type I = Integer

-- | Function that checks whether a string obtained by concatenating two
-- strings: "Dorian" and "Gray" is empty
--
-- Source expression:
-- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
task7Function1 :: Bool
task7Function1 = dotNullHeadMapUncurryIdPPDorianGrey
  where
    dotNullHeadMapUncurryIdPPDorianGrey :: Bool
    dotNullHeadMapUncurryIdPPDorianGrey =
      dollar dotNullHead mapUncurryIdPPDorianGrey
      where
        dotNullHead :: [[Char]] -> Bool
        dotNullHead = dotNull (head :: [[Char]] -> [Char])
          where
            dotNull :: ([[Char]] -> [Char]) -> [[Char]] -> Bool
            dotNull = dot (null :: [Char] -> Bool)
              where
                dot ::
                     ([Char] -> Bool)
                  -> ([[Char]] -> [Char])
                  -> [[Char]]
                  -> Bool
                dot = (.)
        dollar :: ([[Char]] -> Bool) -> [[Char]] -> Bool
        dollar = ($)
        mapUncurryIdPPDorianGrey :: [[Char]]
        mapUncurryIdPPDorianGrey = mapOpUncurryId ppDorianGrey
          where
            mapOpUncurryId :: [([Char] -> [Char], [Char])] -> [[Char]]
            mapOpUncurryId = mapOp uncurryId
              where
                mapOp ::
                     (([Char] -> [Char], [Char]) -> [Char])
                  -> [([Char] -> [Char], [Char])]
                  -> [[Char]]
                mapOp = map
                uncurryId :: ([Char] -> [Char], [Char]) -> [Char]
                uncurryId = uncurryOp idOp
                  where
                    uncurryOp ::
                         (([Char] -> [Char]) -> [Char] -> [Char])
                      -> ([Char] -> [Char], [Char])
                      -> [Char]
                    uncurryOp = uncurry
                    idOp :: ([Char] -> [Char]) -> [Char] -> [Char]
                    idOp = id
            ppDorianGrey :: [([Char] -> [Char], [Char])]
            ppDorianGrey = [(ppDorian, grey)]
              where
                ppDorian = ((++) "Dorian " :: [Char] -> [Char])
                grey = " Grey" :: [Char]

-- | Function that a function that makes a list of pairs (fromLeft, fromRight) 
-- from a list of Lefts and Rights
--
-- Source expression:
-- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
task7Function2 :: [(I, I)]
task7Function2 = zipOpLeftsRights
  where
    zipOpLeftsRights :: [(I, I)]
    zipOpLeftsRights = zipOpLefts rightsOpList
      where
        list :: [Either I I]
        list = [leftPlus, rightPow]
          where
            leftPlus :: Either I anyType1
            leftPlus = Left plusOpNum1Num2
              where
                plusOpNum1Num2 :: I
                plusOpNum1Num2 = plusOpNum1 num2
                  where
                    plusOpNum1 :: I -> I
                    plusOpNum1 = plusOp num1
                      where
                        plusOp :: I -> I -> I
                        plusOp = (+)
                        num1 :: I
                        num1 = 1
                    num2 :: I
                    num2 = 2
            rightPow :: Either anyType2 I
            rightPow = Right powOpNum2Num6
              where
                powOpNum2Num6 :: I
                powOpNum2Num6 = powOpNum2 num6
                  where
                    powOpNum2 :: I -> I
                    powOpNum2 = powOp num2
                      where
                        powOp :: I -> I -> I
                        powOp = (^)
                        num2 :: I
                        num2 = 2
                    num6 :: I
                    num6 = 6
        leftsOpList :: [I]
        leftsOpList = leftsOp list
          where
            leftsOp :: [Either I I] -> [I]
            leftsOp = lefts
        rightsOpList :: [I]
        rightsOpList = rightsOp list
          where
            rightsOp :: [Either I I] -> [I]
            rightsOp = rights
        zipOpLefts :: [I] -> [(I, I)]
        zipOpLefts = zipOp leftsOpList
          where
            zipOp :: [I] -> [I] -> [(I, I)]
            zipOp = zip

-- | function that calculates the implication of a number module 4
-- and the same number module 2
--
-- Source expression:
-- let impl = \x y -> not x || y
--  in let isMod2 = \x -> x `mod` 2 == 0
--   in let isMod4 = \x -> x `mod` 4 == 0
--    in \x -> (isMod4 x) `impl` (isMod2 x)
task7Function3 :: (I -> Bool)
task7Function3 =
  let impl =
        \x y ->
          (((((||) :: Bool -> Bool -> Bool)
               (((not) :: Bool -> Bool) (x :: Bool)) :: Bool -> Bool)
              (y :: Bool)) :: Bool)
   in let isMod2 =
            \x ->
              ((((==) :: I -> I -> Bool)
                  (((((mod) :: I -> I -> I) (x :: I)) :: I -> I)
                   (2 :: I) :: I) :: I -> Bool)
                 (0 :: I)) :: Bool
       in let isMod4 =
                \x ->
                  ((((==) :: I -> I -> Bool)
                      (((((mod) :: I -> I -> I) (x :: I)) :: I -> I)
                       (4 :: I) :: I) :: I -> Bool)
                     (0 :: I)) :: Bool
           in \x ->
                (((impl :: Bool -> Bool -> Bool)
                    (((isMod4 :: I -> Bool) (x :: I)) :: Bool) :: Bool -> Bool)
                   (((isMod2 :: I -> Bool) (x :: I)) :: Bool) :: Bool)
