module Hamcrest.MatchersTests (hamcrestTests) where

import Control.Monad
import Data.Char

import Test.QuickCheck

import Hamcrest.Matchers
import Hamcrest.Assertions (expectThat)

equalToTests :: [Int -> Property]
equalToTests =
    [\x -> expectThat x (equalTo x),
     \x -> expectThat x (not_ (equalTo (x + 1))),

     \x -> expectThat (equalTo x) (describesItselfAs (show x)),
     \x -> expectThat (equalTo 123) (describesAMismatchFor x (show x))]

greaterThanTests :: [Int -> Property]
greaterThanTests =
    [\x -> expectThat x (greaterThan (x - 2)),
     \x -> expectThat x (not_ (greaterThan (x + 1))),
     \x -> expectThat x (not_ (greaterThan x)),

     \x -> expectThat (greaterThan x) (describesItselfAs ("greater than " ++ show x)),
     \x -> expectThat (greaterThan x) (describesAMismatchFor (x - 1) (show $ x - 1))]

lessThanTests :: [Int -> Property]
lessThanTests =
    [\x -> expectThat x (lessThan (x + 4)),
     \x -> expectThat x (not_ (lessThan (x - 1))),
     \x -> expectThat x (not_ (lessThan x)),

     \x -> expectThat (lessThan x) (describesItselfAs ("less than " ++ show x)),
     \x -> expectThat (lessThan x) (describesAMismatchFor (x + 6) (show $ x + 6))]

greaterThanOrEqualToTests :: [Int -> Property]
greaterThanOrEqualToTests =
    [\x -> expectThat x (greaterThanOrEqualTo (x - 8)),
     \x -> expectThat x (not_ (greaterThanOrEqualTo (x + 2))),
     \x -> expectThat x (greaterThanOrEqualTo x),

     \x -> expectThat (greaterThanOrEqualTo x) (describesItselfAs ("greater than or equal to " ++ show x)),
     \x -> expectThat (greaterThanOrEqualTo x) (describesAMismatchFor (x - 9) (show $ x - 9))]

lessThanOrEqualToTests :: [Int -> Property]
lessThanOrEqualToTests =
    [\x -> expectThat x (lessThanOrEqualTo (x + 3)),
     \x -> expectThat x (not_ (lessThanOrEqualTo (x - 4))),
     \x -> expectThat x (lessThanOrEqualTo x),

     \x -> expectThat (lessThanOrEqualTo x) (describesItselfAs ("less than or equal to " ++ show x)),
     \x -> expectThat (lessThanOrEqualTo x) (describesAMismatchFor (x + 1) (show $ x + 1))]

equalToIgnoringCaseTests :: [String -> Property]
equalToIgnoringCaseTests =
    [\x -> expectThat x (equalToIgnoringCase (oscillatingCase x)),
     \x -> expectThat x (equalToIgnoringCase (map toLower x)),
     \x -> expectThat x (equalToIgnoringCase (map toUpper x)),
     \x -> expectThat x (not_ (equalToIgnoringCase (x ++ "a"))),

     \x -> expectThat (equalToIgnoringCase x) (describesItselfAs (show x ++ " ignoring case")),
     \x -> expectThat (equalToIgnoringCase x) (describesAMismatchFor (x ++ "bah") (show (x ++ "bah")))]
    where
    oscillatingCase string = oscillatingCase' True string
    oscillatingCase' _ "" = ""
    oscillatingCase' True (c : cs) = (toUpper c) : oscillatingCase' False cs
    oscillatingCase' False (c : cs) = (toLower c) : oscillatingCase' True cs

tests :: (Show a, Arbitrary a) => [a -> Property] -> IO [Result]
tests = mapM quickCheckResult

hamcrestTests = (liftM concat) $ sequence
    [tests equalToTests,
     tests greaterThanTests,
     tests lessThanTests,
     tests greaterThanOrEqualToTests,
     tests lessThanOrEqualToTests,
     tests equalToIgnoringCaseTests]
