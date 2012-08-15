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
     \x -> expectThat x (not_ (greaterThan x)),

     \x -> expectThat (greaterThan x) (describesItselfAs ("greater than " ++ show x)),
     \x -> expectThat (greaterThan x) (describesAMismatchFor (x - 1) (show $ x - 1))]

lessThanTests =
    [\x -> expectThat x (lessThan (x + 4)),
     \x -> expectThat x (not_ (lessThan (x - 1))),
     \x -> expectThat x (not_ (lessThan x)),

     \x -> expectThat (lessThan x) (describesItselfAs ("less than " ++ show x)),
     \x -> expectThat (lessThan x) (describesAMismatchFor (x + 6) (show $ x + 6))]

greaterThanOrEqualToTests =
    [\x -> expectThat x (greaterThanOrEqualTo (x - 8)),
     \x -> expectThat x (not_ (greaterThanOrEqualTo (x + 2))),
     \x -> expectThat x (greaterThanOrEqualTo x),

     \x -> expectThat (greaterThanOrEqualTo x) (describesItselfAs ("greater than or equal to " ++ show x)),
     \x -> expectThat (greaterThanOrEqualTo x) (describesAMismatchFor (x - 9) (show $ x - 9))]

lessThanOrEqualToTests =
    [\x -> expectThat x (lessThanOrEqualTo (x + 3)),
     \x -> expectThat x (not_ (lessThanOrEqualTo (x - 4))),
     \x -> expectThat x (lessThanOrEqualTo x),

     \x -> expectThat (lessThanOrEqualTo x) (describesItselfAs ("less than or equal to " ++ show x)),
     \x -> expectThat (lessThanOrEqualTo x) (describesAMismatchFor (x + 1) (show $ x + 1))]

hamcrestTests
     = equalToTests
    ++ greaterThanTests
    ++ lessThanTests
    ++ greaterThanOrEqualToTests
    ++ lessThanOrEqualToTests
