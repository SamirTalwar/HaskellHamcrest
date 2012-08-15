module Hamcrest.Matchers where

import Hamcrest

not_ matcher = Matcher { describe = "not " ++ describe matcher,
                         describeMismatch = describeMismatch matcher,
                         matches = Prelude.not . (matches matcher) }

equalTo expected = Matcher (show expected) show (expected ==)

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
