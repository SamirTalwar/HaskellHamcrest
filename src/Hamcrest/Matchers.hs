module Hamcrest.Matchers where

import Hamcrest

not_ matcher = Matcher { describe = "not " ++ describe matcher,
                         describeMismatch = ("not " ++) . (describeMismatch matcher),
                         matches = Prelude.not . (matches matcher) }

equalTo expected = Matcher (show expected) show (expected ==)
