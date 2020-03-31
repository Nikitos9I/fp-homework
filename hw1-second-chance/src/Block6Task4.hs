module Block6Task4
  ( listlistParser
  ) where

import Block6Task1 (Parser (..))
import Block6Task2 (element, ok, satisfy)
import Block6Task3 (parseInt)
import Control.Applicative
import Control.Monad (liftM2, replicateM)
import Data.Char (isSpace)

parseComma :: Parser Char ()
parseComma = () <$ element ','

parseSpace :: Parser Char ()
parseSpace = () <$ many (satisfy isSpace)

skipDelimiters :: Parser Char ()
skipDelimiters = parseSpace *> parseComma *> parseSpace

-- | Parser list of numbers separated by comma
listlistParser :: Parser Char [[Int]]
listlistParser = liftM2 (:) list (many $ skipDelimiters *> list) <|> ([] <$ ok)
  where
    list = parseInt >>= \x -> replicateM x (skipDelimiters *> parseInt)
