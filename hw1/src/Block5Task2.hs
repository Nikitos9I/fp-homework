module Block5Task2
  ( moving
  ) where

--import Control.Monad.State
--import Debug.Trace

type Window = (Integer, Integer)

moveWindow :: [Integer] -> Integer -> Window -> [Float]
moveWindow xs n w@(from, to)
  | to - from + 1 < n = computeNext xs 0 w : moveWindow xs n (from, to + 1)
  | to == toInteger (length xs) = []
  | otherwise = computeNext xs 0 w : moveWindow xs n (from + 1, to + 1)

computeNext :: [Integer] -> Integer -> Window -> Float
computeNext [] _ _ = 0
computeNext (x:xs) i w@(from, to)
  | i == from =
    (fromInteger x + computeNext xs (i + 1) w) / fromInteger (to - from + 1)
  | i > from && i <= to = fromInteger x + computeNext xs (i + 1) w
  | otherwise = computeNext xs (i + 1) w

moving :: Integer -> [Integer] -> [Float]
moving n xs = moveWindow xs n (0, 0)
-- My tries =(
--moveWindow1 :: [Integer] -> Integer -> State Window Window -> [Float]
--moveWindow1 _ _ w | trace ("state = " ++ show (evalState w (0, 0))) False = undefined
--moveWindow1 xs n w
--  | to - from + 1 < n =
--    computeNext xs 0 window :
--    moveWindow1 xs n (withState (\(f, t) -> (f, t + 1)) w)
--  | to == toInteger (length xs) = []
--  | otherwise =
--    computeNext xs 0 window :
--    moveWindow1 xs n (withState (\(f, t) -> (f + 1, t + 1)) w)
--  where
--    window = evalState w (0, 0)
--    from = fst window
--    to = snd window
--type Queue = [Integer]
--
--pop :: State Queue Integer
--pop = state $ \xs -> (last xs, init xs)
--
--push :: Integer -> State Queue ()
--push x = state $ \xs -> ((), x:xs)
--
--popPush :: Integer -> State Queue ()
--popPush x = pop >> push x
--compute :: Integer -> [Integer] -> Integer -> State Queue Int -> [Double]
--compute _ [] _ = do
--    res <- get
--    let fst = head res in
--      return (fromInteger fst) / 1.0
--compute n (x:xs) i = do
--    res <- get
--    let fst = head res
--    if i < n then push x else popPush x
--    compute n xs $ i + 1
