module Hamcrest.MatchersTests (hamcrestTests) where

import Control.Monad
import Data.Char

import Test.QuickCheck

import Hamcrest.Matchers
import Hamcrest.Assertions (expectThat)

equalToTests :: [(String, Int -> Property)]
equalToTests =
    [("x is equal to x",
      \x -> expectThat x (equalTo x)),
     ("x is not equal to (x + 1)",
      \x -> expectThat x (not_ (equalTo (x + 1)))),

     ("(equalTo x) describes itself as \"x\"",
      \x -> expectThat (equalTo x) (describesItselfAs (show x))),
     ("(equalTo x) describes a mismatch for y as \"y\"",
      \x -> expectThat (equalTo 123) (describesAMismatchFor x (show x)))]

greaterThanTests :: [(String, Int -> Property)]
greaterThanTests =
    [("x is greater than (x - 2)",
      \x -> expectThat x (greaterThan (x - 2))),
     ("x is not greater than (x + 1)",
      \x -> expectThat x (not_ (greaterThan (x + 1)))),
     ("x is not greater than x",
      \x -> expectThat x (not_ (greaterThan x))),

     ("(greaterThan x) describes itself as \"greater than x\"",
      \x -> expectThat (greaterThan x) (describesItselfAs ("greater than " ++ show x))),
     ("(greaterThan x) describes a mismatch for (x + 1) as \"(x + 1)\"",
      \x -> expectThat (greaterThan x) (describesAMismatchFor (x + 1) (show $ x + 1)))]

lessThanTests :: [(String, Int -> Property)]
lessThanTests =
    [("x is less than (x + 4)",
      \x -> expectThat x (lessThan (x + 4))),
     ("x is not less than (x - 1)",
      \x -> expectThat x (not_ (lessThan (x - 1)))),
     ("x is not less than x",
      \x -> expectThat x (not_ (lessThan x))),

     ("(lessThan x) describes itself as \"less than x\"",
      \x -> expectThat (lessThan x) (describesItselfAs ("less than " ++ show x))),
     ("(lessThan x) describes a mismatch for (x - 6) as \"(x - 6)\"",
      \x -> expectThat (lessThan x) (describesAMismatchFor (x - 6) (show $ x - 6)))]

greaterThanOrEqualToTests :: [(String, Int -> Property)]
greaterThanOrEqualToTests =
    [("x is greater than or equal to (x - 8)",
      \x -> expectThat x (greaterThanOrEqualTo (x - 8))),
     ("x is not greater than or equal to (x + 2)",
      \x -> expectThat x (not_ (greaterThanOrEqualTo (x + 2)))),
     ("x is greater than or equal to x",
      \x -> expectThat x (greaterThanOrEqualTo x)),

     ("(greaterThanOrEqualTo x) describes itself as \"greater than or equal to x\"",
      \x -> expectThat (greaterThanOrEqualTo x) (describesItselfAs ("greater than or equal to " ++ show x))),
     ("(greaterThanOrEqualTo x) describes a mismatch for (x + 9) as \"(x + 9)\"",
      \x -> expectThat (greaterThanOrEqualTo x) (describesAMismatchFor (x + 9) (show $ x + 9)))]

lessThanOrEqualToTests :: [(String, Int -> Property)]
lessThanOrEqualToTests =
    [("x is less than or equal to (x + 3)",
      \x -> expectThat x (lessThanOrEqualTo (x + 3))),
     ("x is not less than or equal to (x - 4)",
      \x -> expectThat x (not_ (lessThanOrEqualTo (x - 4)))),
     ("x is less than or equal to x",
      \x -> expectThat x (lessThanOrEqualTo x)),

     ("(lessThanOrEqualTo x) describes itself as \"less than or equal to x\"",
      \x -> expectThat (lessThanOrEqualTo x) (describesItselfAs ("less than or equal to " ++ show x))),
     ("(lessThanOrEqualTo x) describes a mismatch for (x - 1) as \"(x - 1)\"",
      \x -> expectThat (lessThanOrEqualTo x) (describesAMismatchFor (x - 1) (show $ x - 1)))]

equalToIgnoringCaseTests :: [(String, String -> Property)]
equalToIgnoringCaseTests =
    [("\"ABCdef\" is equal to \"AbCdEf\" ignoring case",
      \x -> expectThat x (equalToIgnoringCase (oscillatingCase x))),
     ("\"ABCdef\" is equal to \"abcdef\" ignoring case",
      \x -> expectThat x (equalToIgnoringCase (map toLower x))),
     ("\"ABCdef\" is equal to \"ABCDEF\" ignoring case",
      \x -> expectThat x (equalToIgnoringCase (map toUpper x))),
     ("\"ABCdef\" is not equal to \"ABCdefGHI\"",
      \x -> expectThat x (not_ (equalToIgnoringCase (x ++ "a")))),

     ("(equalToIgnoringCase x) describes itself as \"x ignoring case\"",
      \x -> expectThat (equalToIgnoringCase x) (describesItselfAs (show x ++ " ignoring case"))),
     ("(equalToIgnoringCase x) describes a mismatch for y as \"y\"",
      \x -> expectThat (equalToIgnoringCase x) (describesAMismatchFor (x ++ "bah") (show (x ++ "bah"))))]
    where
    oscillatingCase string = oscillatingCase' True string
    oscillatingCase' _ "" = ""
    oscillatingCase' True (c : cs) = (toUpper c) : oscillatingCase' False cs
    oscillatingCase' False (c : cs) = (toLower c) : oscillatingCase' True cs

tests :: (Show a, Arbitrary a) => [(String, a -> Property)] -> IO [Result]
tests cases = do
    mapM quickCheckResult $ map (uncurry label) cases

hamcrestTests = (liftM concat) $ sequence
    [tests equalToTests,
     tests greaterThanTests,
     tests lessThanTests,
     tests greaterThanOrEqualToTests,
     tests lessThanOrEqualToTests,
     tests equalToIgnoringCaseTests]
