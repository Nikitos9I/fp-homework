module Block6Task1 where

import Control.Applicative

newtype Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

parser :: ([s] -> Maybe (a, [s])) -> Parser s a
parser p = Parser {runParser = p}

instance Functor (Parser s) where
  fmap f p =
    parser $ \x -> do
      (a, rest) <- runParser p x
      return (f a, rest)

instance Applicative (Parser s) where
  pure a = parser $ \x -> return (a, x)
  pAB <*> pA =
    parser $ \x -> do
      (ab, s1) <- runParser pAB x
      (a, s2) <- runParser pA s1
      return (ab a, s2)

instance Monad (Parser s) where
  return = pure
  pA >>= f =
    parser $ \x -> do
      (a, s1) <- runParser pA x
      runParser (f a) s1

instance Alternative (Parser s) where
  empty = parser $ const Nothing
  f <|> g = parser $ \x -> runParser f x <|> runParser g x
