module Block6Task2
  ( ok
  , eof
  , satisfy
  , element
  , stream
  ) where

import Block6Task1

-- | Parse stream and return it
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Parse stream and return Just if input is end of stream and Nothing else
eof :: Parser s ()
eof =
  Parser $ \s ->
    if null s
      then Just ((), s)
      else Nothing

-- | Parse stream and return Just if predicate is success
-- for head stream element and Nothing else
satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \s ->
    if not (null s) && p (head s)
      then Just (head s, tail s)
      else Nothing

-- | Parse stream and return Just if head stream element is equal to
-- first parameter and Nothing else
element :: Eq s => s -> Parser s s
element el = satisfy (== el)

-- | Parse stream and return Just if init stream elements is equal to
-- first parameter and Nothing else
stream :: Eq s => [s] -> Parser s [s]
stream = traverse element
