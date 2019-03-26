{-# LANGUAGE DerivingStrategies, DerivingVia #-}

module LeapYearVia (isLeapYear) where

import Data.Coerce (coerce)
import Data.Semigroup


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

--------------------------------
-- first iteration, passed tests
--------------------------------
--isLeapYear :: Integer -> Bool
--isLeapYear year =
--  coerce (byFour year) &&
--  coerce (notHundred year) ||
--  coerce (exceptFourHundred year)

------------------------------------
-- trying with a semigroup
------------------------------------

newtype LeapYear = LeapYear Bool
  deriving Show
  deriving (Eq, Semigroup) via All

-- this is dumb and probably a sign that i have introduced unnecessary complexity lol
hundreds :: Integer -> LeapYear
hundreds x = coerce ((coerce (notHundred x)) || (coerce (exceptFourHundred x)))

isLeapYear :: Integer -> Bool
isLeapYear year =
  coerce (byFour year <> hundreds year)

