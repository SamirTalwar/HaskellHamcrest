module Hamcrest.MatchersTests (hamcrestTests) where

import Test.QuickCheck (Property)

import Hamcrest.Matchers
import Hamcrest.Assertions (expectThat)

equalToTests =
    [\x -> expectThat x (equalTo x),
     \x -> expectThat x (not_ (equalTo (x + 1))),

     \x -> expectThat (equalTo x) (describesItselfAs (show x)),
     \x -> expectThat (equalTo 123) (describesAMismatchFor x (show x))]

greaterThanTests =
    [\x -> expectThat x (greaterThan (x - 2)),
     \x -> expectThat x (not_ (greaterThan (x + 1))),

     \x -> expectThat (greaterThan x) (describesItselfAs ("greater than " ++ show x)),
     \x -> expectThat (greaterThan x) (describesAMismatchFor (x - 1) (show $ x - 1))]

hamcrestTests = equalToTests ++ greaterThanTests
