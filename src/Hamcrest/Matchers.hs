module Hamcrest.Matchers where

import Data.Char

import Hamcrest

not_ matcher = Matcher
    { describe = "not " ++ describe matcher,
      describeMismatch = describeMismatch matcher,
      matches = Prelude.not . (matches matcher) }

is expected = Matcher ("is " ++ show expected) (("was " ++) . show) (expected ==)

equalTo expected = Matcher (show expected) show (expected ==)

greaterThan value = Matcher ("greater than " ++ show value) show (> value)
lessThan value = Matcher ("less than " ++ show value) show (< value)
greaterThanOrEqualTo value = Matcher ("greater than or equal to " ++ show value) show (>= value)
lessThanOrEqualTo value = Matcher ("less than or equal to " ++ show value) show (<= value)

equalToIgnoringCase expected = Matcher
    { describe = show expected ++ " ignoring case",
      describeMismatch = show,
      matches = \actual -> upperCase expected == upperCase actual }
    where
    upperCase = map toUpper

describesItselfAs :: (Show a) => String -> Matcher (Matcher a)
describesItselfAs expected = Matcher
    { describe = "describes itself as " ++ (show expected),
      describeMismatch = ("described itself as " ++) . describe,
      matches = (expected ==) . describe }

describesAMismatchFor :: (Show a) => a -> String -> Matcher (Matcher a)
describesAMismatchFor value expected = Matcher
    { describe = "describes a mismatch for " ++ (show value) ++ " as " ++ (show expected),
      describeMismatch = \matcher -> "described the mismatch as " ++ describeMismatch matcher value,
      matches = \matcher -> expected == describeMismatch matcher value }
