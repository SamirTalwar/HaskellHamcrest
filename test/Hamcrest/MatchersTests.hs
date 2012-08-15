module Hamcrest.MatchersTests (hamcrestTests) where

import Test.QuickCheck (Property)

import Hamcrest.Matchers
import Hamcrest.Assertions (expectThat)

equalToTests :: [Int -> Property]
equalToTests = [\x -> expectThat x (equalTo x),
                \x -> expectThat x (not_ (equalTo (x + 1))),

                \x -> expectThat (equalTo x) (describesItselfAs (show x)),
                \x -> expectThat (equalTo 123) (describesAMismatchFor x (show x))]

hamcrestTests = equalToTests
