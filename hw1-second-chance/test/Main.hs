module Main where

import Control.Applicative
import Control.Exception (evaluate)
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.HUnit

import Block6Task1
import Block6Task2
import Block6Task3
import Block6Task4

main :: IO ()
main = defaultMain runTests

runTests :: TestTree
runTests = testGroup "All unit tests" [task1, task2, task3, task4]

task1 :: TestTree
task1 =
  testGroup
    "Task 1"
    [ testCase "Test functor" $
      runParser (fmap (+ 2) parseInt) "2" @?= Just (4, "")
    , testCase "Test applicative" $ pure "asd" @?= Just "asd"
    , testCase "Test monad" $
      runParser
        (parseInt >>= \x -> (fmap . replicate) x (element ' ' *> parseInt))
        "2 3" @?=
      Just ([3, 3], "")
    , testCase "Test alternative +" $
      runParser (element '-' <|> element '+') "+ad" @?= Just ('+', "ad")
    , testCase "Test alternative -" $
      runParser (element '-' <|> element '+') "-ad" @?= Just ('-', "ad")
    ]

task2 :: TestTree
task2 =
  testGroup
    "Task 2"
    [ testCase "Test ok parser" $
      runParser ok "asdasdassfs" @?= Just ((), "asdasdassfs")
    , testCase "Test eof parser nothing" $
      runParser eof "asdasdassfs" @?= Nothing
    , testCase "Test eof parser just" $ runParser eof "" @?= Just ((), "")
    , testCase "Test satisfy parser just" $
      runParser (satisfy (== 'c')) "casd" @?= Just ('c', "asd")
    , testCase "Test satisfy parser nothing" $
      runParser (satisfy (== 'c')) "aasd" @?= Nothing
    , testCase "Test element parser just" $
      runParser (element 'c') "casd" @?= Just ('c', "asd")
    , testCase "Test element parser nothing" $
      runParser (element 'c') "aasd" @?= Nothing
    , testCase "Test stream parser just" $
      runParser (stream "cas") "casd" @?= Just ("cas", "d")
    , testCase "Test stream parser nothing" $
      runParser (stream "opop") "casd" @?= Nothing
    ]

task3 :: TestTree
task3 =
  testGroup
    "Task 3"
    [ testCase "Test parseBrackets ()()()" $
      runParser parseBrackets "()()()" @?= Just ("()()()", "")
    , testCase "Test parseBrackets ()a()()" $
      runParser parseBrackets "()a()()" @?= Just ("()", "a()()")
    , testCase "Test parseBrackets ((((" $
      runParser parseBrackets "((((" @?= Nothing
    , testCase "Test parseBrackets ))))" $
      runParser parseBrackets "))))" @?= Nothing
    , testCase "Test parseBrackets ()(()(()((((())))()())))" $
      runParser parseBrackets "()(()(()((((())))()())))" @?=
      Just ("()(()(()((((())))()())))", "")
    , testCase "Test parseBrackets (as)" $
      runParser parseBrackets "(as)" @?= Nothing
    , testCase "Test parseBrackets 1" $ runParser parseBrackets "1" @?= Nothing
    , testCase "Test parseBracketsWithState ()()()" $
      runParser parseBracketsWithState "()()()" @?= Just ("()()()", "")
    , testCase "Test parseBracketsWithState ()a()()" $
      runParser parseBracketsWithState "()a()()" @?= Just ("()", "a()()")
    , testCase "Test parseBracketsWithState ((((" $
      runParser parseBracketsWithState "((((" @?= Nothing
    , testCase "Test parseBracketsWithState ))))" $
      runParser parseBracketsWithState "))))" @?= Nothing
    , testCase "Test parseBracketsWithState ()(()(()((((())))()())))" $
      runParser parseBracketsWithState "()(()(()((((())))()())))" @?=
      Just ("()(()(()((((())))()())))", "")
    , testCase "Test parseBracketsWithState (as)" $
      runParser parseBracketsWithState "(as)" @?= Nothing
    , testCase "Test parseBracketsWithState 1" $
      runParser parseBracketsWithState "1" @?= Nothing
    , testCase "Test parseInt +12" $ runParser parseInt "+12" @?= Just (12, "")
    , testCase "Test parseInt -122" $
      runParser parseInt "-122" @?= Just (-122, "")
    , testCase "Test parseInt +0" $ runParser parseInt "+0" @?= Just (0, "")
    , testCase "Test parseInt 444asd" $
      runParser parseInt "444asd" @?= Just (444, "asd")
    , testCase "Test parseInt -123 asd" $
      runParser parseInt "-123 asd" @?= Just (-123, " asd")
    ]

task4 :: TestTree
task4 =
  testGroup
    "Task 4"
    [ testCase "Test listlistParser 2, 1,+10  , 3,5,-7, 2" $
      runParser listlistParser "2, 1,+10  , 3,5,-7, 2" @?=
      Just ([[1, 10], [5, -7, 2]], "")
    , testCase "Test listlistParser 2, 1a,+10  , 3,5,-7, 2" $
      runParser listlistParser "2, 1a,+10  , 3,5,-7, 2" @?=
      Just ([], "2, 1a,+10  , 3,5,-7, 2")
    , testCase "Test listlistParser 6, 1,+10  , 3,5,-7, 2" $
      runParser listlistParser "6, 1,+10  , 3,5,-7, 2" @?=
      Just ([[1, 10, 3, 5, -7, 2]], "")
    , testCase "Test listlistParser 3a, 2,1,-1" $
      runParser listlistParser "3a, 2,1,-1" @?= Just ([], "3a, 2,1,-1")
    ]
