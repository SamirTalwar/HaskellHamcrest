module Hamcrest.Assertions where

import Test.QuickCheck

import Hamcrest.Matchers

expectThat :: a -> Matcher a -> Property
expectThat actual matcher = do
    printTestCase description $ matcher `matches` actual
    where
    description = "Expected: " ++ describe matcher ++ "\n"
               ++ "But: " ++ (describeMismatch matcher) actual ++ "\n"
