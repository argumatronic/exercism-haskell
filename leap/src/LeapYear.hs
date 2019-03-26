module LeapYear (isLeapYear) where

import Data.Coerce (coerce)

newtype LeapYear = LeapYear Bool
  deriving Show

byFour :: Integer -> LeapYear
byFour x = if (x `rem` 4 == 0)
           then (LeapYear True)
           else (LeapYear False)

notHundred :: Integer -> LeapYear
notHundred x = if (x `rem` 100 == 0)
               then (LeapYear False)
               else (LeapYear True)

exceptFourHundred :: Integer -> LeapYear
exceptFourHundred x = if (x `rem` 400 == 0)
                      then (LeapYear True)
                      else (LeapYear False)

isLeapYear :: Integer -> Bool
isLeapYear year =
  coerce (byFour year) &&
  coerce (notHundred year) ||
  coerce (exceptFourHundred year)


