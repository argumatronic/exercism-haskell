module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)
import Data.Maybe (mapMaybe)


sorter :: String -> String -> Bool
sorter xs ys = (sort . map toLower) xs == (sort . map toLower) ys

isNotSame :: String -> String -> Bool
isNotSame xs ys = map toLower xs /= map toLower ys

isAnagram :: String -> String -> Bool
isAnagram xs ys = (sorter xs ys) && (isNotSame xs ys)

---------------------------
-- filter version
---------------------------

anagramsFor :: String -> [String] -> [String]
anagramsFor xs ys =
  filter (isAnagram xs) ys

--------------------------
-- "test data"
--------------------------

testList :: [String]
testList = ["enlists", "google", "Inlets", "banana", "listen"]

testList2 :: [String]
testList2 = ["chris", "uliej", "alonzo", "eiluJ", "julies", " ", "Julie"]

--------------------------
-- mapMaybe version
--------------------------

isAna :: String -> String -> Maybe String
isAna xs ys =
  case sorter xs ys of
    False -> Nothing
    True ->
      case isNotSame xs ys of
        True -> Just ys
        False -> Nothing

anagramsForMay :: String -> [String] -> [String]
anagramsForMay xs yss = mapMaybe (isAna xs) yss
