module Main where

import Control.Exception (evaluate)
import Data.List.NonEmpty hiding (fromList)
import Data.Maybe (isJust, isNothing)
import Data.Monoid hiding (Endo)
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.HUnit

import Block1Task1
import Block1Task2
import Block1Task3
import Block2Task2
import Block3Task1
import Block3Task2
import Block4Task1
import qualified Block4Task2 as B4T2

main :: IO ()
main = defaultMain runTests

runTests :: TestTree
runTests = testGroup "All unit tests" [block1, block2, block3, block4]

block1 :: TestTree
block1 = testGroup "Block1 tests" [b1t1, b1t2, b1t3]

block2 :: TestTree
block2 = testGroup "Block2 tests" [b2t2]

block3 :: TestTree
block3 = testGroup "Block3 tests" [b3t1, b3t2]

block4 :: TestTree
block4 = testGroup "Block4 tests" [b4t1, b4t2]

b1t1 :: TestTree
b1t1 =
  testGroup
    "Task 1"
    [ testCase "Next day after Sunday is Monday" $ nextDay Sunday @?= Monday
    , testCase "Next day after Wednesday is Thursday" $
      nextDay Wednesday @?= Thursday
    , testCase "Saturday is weekend" $ isWeekend Saturday @?= True
    , testCase "Tuesday is not weekend" $ isWeekend Tuesday @?= False
    , testCase "After Saturday 3 is Tuesday" $ afterDays Saturday 3 @?= Tuesday
    , testCase "After Sunday 14 is Sunday" $ afterDays Sunday 14 @?= Sunday
    , testCase "After Tuesday 1 is nextDay Tuesday" $
      afterDays Tuesday 1 @?= nextDay Tuesday
    , testCase "After Tuesday is 3 days to Friday" $ daysToParty Tuesday @?= 3
    , testCase "After Friday is 0 days to Friday" $ daysToParty Friday @?= 0
    , testCase "After Saturday is 6 days to Friday" $ daysToParty Saturday @?= 6
    ]

b1t2 :: TestTree
b1t2 =
  testGroup
    "Task 2"
    [ testCase "Equal two naturals" $ S Z `equalNat` S Z @?= True
    , testCase "Check correct sum work" $
      (S $ S $ S Z) `sumNat` (S $ S Z) `equalNat` (S $ S $ S $ S $ S Z) @?= True
    , testCase "Check correct sub work" $
      (S $ S $ S Z) `subNat` (S $ S Z) `equalNat` S Z @?= True
    , testCase "Check incorrect sub work" $
      evaluate ((S $ S $ S Z) `subNat` (S $ S $ S $ S $ S Z)) `shouldThrow`
      anyErrorCall
    , testCase "Check non-zero times zero" $
      (S $ S $ S Z) `mulNat` Z `equalNat` Z @?= True
    , testCase "Check non-zero times non-zero" $
      (S $ S $ S Z) `mulNat` (S $ S Z) `equalNat` (S $ S $ S $ S $ S $ S Z) @?=
      True
    , testCase "Convert positive Int to Nat" $
      convertIntToNat 5 `equalNat` (S $ S $ S $ S $ S Z) @?= True
    , testCase "Convert negative Int to Nat" $
      evaluate (convertIntToNat $ -10) `shouldThrow` anyErrorCall
    , testCase "Convert Nat to Int" $
      convertNatToInt (S $ S $ S $ S $ S $ S $ S Z) @?= 7
    , testCase "Check two Nats are equal" $
      equalNat (S $ S $ S $ S Z) (S $ S $ S $ S Z) @?= True
    , testCase "Check two Nats are not equal" $
      equalNat (S $ S $ S $ S Z) (S $ S Z) @?= False
    , testCase "Comparing two Nats and the first is greather" $
      compareNat (S $ S $ S $ S $ S Z) (S $ S Z) @?= 1
    , testCase "Comparing equal Nats" $ compareNat (S $ S Z) (S $ S Z) @?= 0
    , testCase "Comparing two Nats and the first is smaller" $
      compareNat (S $ S Z) (S $ S $ S $ S $ S Z) @?= -1
    , testCase "Check Parity of even Nat" $ parityNat (S $ S Z) @?= True
    , testCase "Check Parity of odd Nat" $ parityNat (S $ S $ S Z) @?= False
    ]

b1t3 :: TestTree
b1t3 =
  testGroup
    "Task 3"
    [ testCase "Check empty tree" $ isEmpty Leaf @?= True
    , testCase "Check not empty tree" $
      isEmpty (Node (One 1) Leaf Leaf) @?= False
    , testCase "Check size of tree is 6" $
      countSize
        (Node
           (Many 4 $ Many 4 $ One 4)
           (Node (Many 2 $ One 2) Leaf Leaf)
           (Node (One 6) Leaf Leaf)) @?=
      6
    , testCase "Check size of tree is 2" $
      countSize (Node (Many 2 $ One 2) Leaf Leaf) @?= 2
    , testCase "Check element exists in tree" $
      findElem
        (2 :: Int)
        (Node (Many 4 $ Many 4 $ One 4) (Node (Many 2 $ One 2) Leaf Leaf) Leaf) `shouldSatisfy`
      isJust
    , testCase "Check element does not exist in tree" $
      findElem
        (8 :: Int)
        (Node (Many 4 $ Many 4 $ One 4) (Node (Many 2 $ One 2) Leaf Leaf) Leaf) `shouldSatisfy`
      isNothing
    , testCase "Insert element in tree with node extension" $
      insertElem (2 :: Int) (Node (Many 2 $ One 2) Leaf Leaf) @?=
      Node (Many 2 $ Many 2 $ One 2) Leaf Leaf
    , testCase "Insert element in tree with adding new node" $
      insertElem
        (5 :: Int)
        (Node
           (Many 4 $ Many 4 $ One 4)
           (Node (Many 2 $ One 2) Leaf Leaf)
           (Node (One 6) Leaf Leaf)) @?=
      Node
        (Many 4 $ Many 4 $ One 4)
        (Node (Many 2 $ One 2) Leaf Leaf)
        (Node (One 6) (Node (One 5) Leaf Leaf) Leaf)
    , testCase "Get tree from list" $
      fromList [4, 2, 6, 5, 4, 2, 4] @?=
      Node
        (Many 4 $ Many 4 $ One 4)
        (Node (Many 2 $ One 2) Leaf Leaf)
        (Node (One 6) (Node (One 5) Leaf Leaf) Leaf)
    , testCase "Get empty tree from empty list" $
      fromList ([] :: [Int]) @?= Leaf
    , testCase "Remove element from tree with node reduction" $
      removeElem 4 (Node (Many 4 $ Many 4 $ One 4) Leaf Leaf) @?=
      Node (Many 4 $ One 4) Leaf Leaf
    , testCase "Remove element from tree with node removing" $
      removeElem
        3
        (Node
           (One 5)
           (Node (One 3) (Node (One 2) Leaf Leaf) (Node (One 4) Leaf Leaf))
           Leaf) @?=
      Node (One 5) (Node (One 2) Leaf (Node (One 4) Leaf Leaf)) Leaf
    ]

b2t2 :: TestTree
b2t2 =
  testGroup
    "Task 2"
    [ testCase "Check `split on` function" $
      splitOn '/' "path/to/file" @?=
      ("path" Data.List.NonEmpty.:| ["to", "file"])
    , testCase "Check `join with` function" $
      joinWith '/' ("path" Data.List.NonEmpty.:| ["to", "file"]) @?=
      "path/to/file"
    , testCase "joinWith x . splitOn x ≡ id" $
      joinWith '/' (splitOn '/' "path/to/file") @?= "path/to/file"
    ]

b3t1 :: TestTree
b3t1 =
  testGroup
    "Task 1"
    [ testCase "Check `maybeConcat` function" $
      maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]] @?= [1, 2, 3, 4, 5]
    , testCase "Check `eitherConcat` function" $
      eitherConcat [Left (Sum 3), Right [1, 2, 3], Left (Sum 5), Right [4, 5]] @?=
      (Sum {getSum = 8}, [1, 2, 3, 4, 5])
    ]

b3t2 :: TestTree
b3t2 =
  testGroup
    "Task 2"
    [ testCase "Check NonEmpty Semigroup" $
      "a" Block3Task2.:| ["ti", "uzhe", "sdelal"] <> "dz" Block3Task2.:| ["po"] <>
      "func" Block3Task2.:|
      ["programming"] @?=
      "adzfunc" Block3Task2.:|
      ["ti", "uzhe", "sdelal", "po", "programming"]
    , testCase "Check ThisOrThat Semigroup (This That)" $
      This "ti" <> That "9i" @?= Both "ti" "9i"
    , testCase "Check ThisOrThat Semigroup (This Both)" $
      This "ti" <> Both "or" "me" @?= Both "tior" "me"
    , testCase "Check ThisOrThat Semigroup (Both Both)" $
      Both "ko" "po" <> Both "ar" "fa" @?= Both "koar" "pofa"
    , testCase "Check Name Semigroup" $
      Name "root" <> Name "server" @?= Name "root.server"
    , testCase "Check Name Monoid" $
      Name "root" <> Name mempty <> Name "hoho" @?= Name "root..hoho"
--    , testCase "Check Endo Semigroup" $
--      Endo {getEndo = id "a"} <> Endo "b" <> Endo "c" @?= Endo "abc"
    ]

b4t1 :: TestTree
b4t1 =
  testGroup "Task 1"
    [ testCase "Check string sum valid" $
      stringSum "12 50 100.2" @?= Just 162.2
    , testCase "Check string sum invalid1" $
      stringSum "12.213 42 a231" @?= Nothing
    , testCase "Check string sum invalid2" $
      stringSum "1a2" @?= Nothing
    ]

b4t2 :: TestTree
b4t2 = testGroup "Task2"
    [ --testCase "Check tree functor" $
--      fmap (* (2 :: Int)) (B4T2.Branch (B4T2.Leaf (4 :: Int)) (B4T2.Leaf (2 :: Int))) @?= 8 :: Int
--    testCase "Check tree applicative" $
--      ((B4T2.Branch (B4T2.Leaf (+ 1)) (B4T2.Leaf (+ 2)) <*> B4T2.Leaf 7) :: B4T2.Tree) @?= ((B4T2.Branch (B4T2.Leaf 8) (B4T2.Leaf 9)) :: B4T2.Tree)
    ]