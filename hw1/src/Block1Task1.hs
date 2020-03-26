module Block1Task1
  ( WeekDay(..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

import Data.List (elemIndex)
import Data.Maybe

-- | Week day in Russian calendar rules
data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Eq WeekDay where
  (==) day1 day2 = show day1 == show day2

list :: [WeekDay]
list = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

convertDayToNum :: WeekDay -> Int
convertDayToNum x = fromJust (elemIndex x list)

convertNumToDay :: Int -> WeekDay
convertNumToDay i =
  if i >= 0 && i < 7
    then list !! i
    else error "Invalid index"

-- | Return next day in Russian calendar rules
nextDay :: WeekDay -> WeekDay
nextDay x = convertNumToDay $ (convertDayToNum x + 1) `mod` 7

-- | Return next day after number of days in Russian calendar rules
afterDays :: WeekDay -> Int -> WeekDay
afterDays x n = convertNumToDay $ (convertDayToNum x + n) `mod` 7

-- | Check if @param day is weekend in Russian calendar rules
isWeekend :: WeekDay -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Return number days from @param day to friday in Russian calendar rules
daysToParty :: WeekDay -> Int
daysToParty x = (convertDayToNum Friday - convertDayToNum x + 7) `mod` 7
