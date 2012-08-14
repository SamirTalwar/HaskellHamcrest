module Hamcrest.Matchers where

import qualified Hamcrest.Matchers.EqualTo as EqualTo
import qualified Hamcrest.Matchers.Not as Not

not_ = Not.not

equalTo :: (Eq a) => a -> a -> Bool
equalTo = EqualTo.equalTo
