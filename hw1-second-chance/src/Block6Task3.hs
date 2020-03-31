module Block6Task3
  ( parseInt
  , parseBrackets
  , parseBracketsWithState
  ) where

import Block6Task1
import Block6Task2
import Control.Applicative
import Control.Monad.State
import Data.Char (isDigit)

type Stack = String

-- | Valid prefix, rest, accumulator
type Spanner = (String, String, String)

type Data = (Spanner, Stack)

spanBracket :: String -> State Data Spanner
spanBracket [] = do
  (spanner, _) <- get
  return spanner
spanBracket (x:xs) = do
  ((pref, rest, acc), balance) <- get
  let upd ch st
        | ch == '(' = '(' : st
        | not (null st) && head st == '(' && ch == ')' = tail st
        | otherwise = ')' : st
   in case x of
        '(' -> put ((pref, rest, acc ++ [x]), upd '(' balance)
        ')' ->
          if null updBalance
            then put ((acc ++ [x], xs, acc ++ [x]), updBalance)
            else put ((pref, rest, acc ++ [x]), updBalance)
          where updBalance = upd ')' balance
        _ -> put ((pref, rest, xs), upd '!' balance)
  spanBracket xs

-- | Parse string of brackets to valid brackets sequence safety
-- This version was implemented instead of Task 2 in Block 5,
-- because I understood State monad few moments ago
-- @return valid brackets sequence and rest of input string or nothing
parseBracketsWithState :: Parser Char String
parseBracketsWithState = Parser f
  where
    f xs
      | null pref = Nothing
      | otherwise = Just (pref, rest)
      where
        (pref, rest, _) = evalState (spanBracket xs) (("", "", ""), "")

-- | Parse string of brackets to valid brackets sequence safety
-- if it is correctness and Nothing else
parseBrackets :: Parser Char String
parseBrackets = do
  ob <- element '('
  inner <- many parseBrackets
  cb <- element ')'
  outer <- many parseBrackets
  return $ [ob] ++ join inner ++ [cb] ++ join outer

-- | Parse string to Int safety
parseInt :: Parser Char Int
parseInt =
  read <$>
  ((fmap (:) (element '-') <|> (id <$ element '+') <|> (id <$ ok)) <*>
   some (satisfy isDigit))
