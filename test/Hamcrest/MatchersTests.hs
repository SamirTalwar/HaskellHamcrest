module Hamcrest.MatchersTests (hamcrestTests) where

import Control.Monad
import Data.Char

import Test.QuickCheck

import Hamcrest.Matchers
import Hamcrest.Assertions (expectThat)

isTests :: [(String, (Int, Int) -> Property)]
isTests =
    [("x is x",
      \(x, _) -> expectThat x (is x)),

     ("(is x) describes itself as \"is x\"",
      \(x, _) -> expectThat (is x) (describesItselfAs ("is " ++ show x))),
     ("(is x) describes a mismatch for y as \"was y\"",
      \(x, y) -> expectThat (is x) (describesAMismatchFor y ("was " ++ show y)))]

equalToTests :: [(String, (Int, Int) -> Property)]
equalToTests =
    [("x is equal to x",
      \(x, _) -> expectThat x (equalTo x)),
     ("x is not equal to y",
      \(x, y) -> x /= y ==> expectThat x (not_ (equalTo y))),

     ("(equalTo x) describes itself as \"x\"",
      \(x, _) -> expectThat (equalTo x) (describesItselfAs (show x))),
     ("(equalTo x) describes a mismatch for y as \"y\"",
      \(x, y) -> expectThat (equalTo x) (describesAMismatchFor y (show y)))]

greaterThanTests :: [(String, (Int, Int) -> Property)]
greaterThanTests =
    [("x is greater than (x - n)",
      \(x, y) -> x > y ==> expectThat x (greaterThan y)),
     ("x is not greater than (x + n)",
      \(x, y) -> x < y ==> expectThat x (not_ (greaterThan y))),
     ("x is not greater than x",
      \(x, _) -> expectThat x (not_ (greaterThan x))),

     ("(greaterThan x) describes itself as \"greater than x\"",
      \(x, _) -> expectThat (greaterThan x) (describesItselfAs ("greater than " ++ show x))),
     ("(greaterThan x) describes a mismatch for y as \"y\"",
      \(x, y) -> expectThat (greaterThan x) (describesAMismatchFor y (show y)))]

lessThanTests :: [(String, (Int, Int) -> Property)]
lessThanTests =
    [("x is less than (x + n)",
      \(x, y) -> x < y ==> expectThat x (lessThan y)),
     ("x is not less than (x - n)",
      \(x, y) -> x > y ==> expectThat x (not_ (lessThan y))),
     ("x is not less than x",
      \(x, _) -> expectThat x (not_ (lessThan x))),

     ("(lessThan x) describes itself as \"less than x\"",
      \(x, _) -> expectThat (lessThan x) (describesItselfAs ("less than " ++ show x))),
     ("(lessThan x) describes a mismatch for y as \"y\"",
      \(x, y) -> expectThat (lessThan x) (describesAMismatchFor y (show y)))]

greaterThanOrEqualToTests :: [(String, (Int, Int) -> Property)]
greaterThanOrEqualToTests =
    [("x is greater than or equal to (x - n)",
      \(x, y) -> x > y ==> expectThat x (greaterThanOrEqualTo y)),
     ("x is not greater than or equal to (x + n)",
      \(x, y) -> x < y ==> expectThat x (not_ (greaterThanOrEqualTo y))),
     ("x is greater than or equal to x",
      \(x, _) -> expectThat x (greaterThanOrEqualTo x)),

     ("(greaterThanOrEqualTo x) describes itself as \"greater than or equal to x\"",
      \(x, _) -> expectThat (greaterThanOrEqualTo x) (describesItselfAs ("greater than or equal to " ++ show x))),
     ("(greaterThanOrEqualTo x) describes a mismatch for y as \"y\"",
      \(x, y) -> expectThat (greaterThanOrEqualTo x) (describesAMismatchFor y (show y)))]

lessThanOrEqualToTests :: [(String, (Int, Int) -> Property)]
lessThanOrEqualToTests =
    [("x is less than or equal to (x + n)",
      \(x, y) -> x < y ==> expectThat x (lessThanOrEqualTo y)),
     ("x is not less than or equal to (x - n)",
      \(x, y) -> x > y ==> expectThat x (not_ (lessThanOrEqualTo y))),
     ("x is less than or equal to x",
      \(x, _) -> expectThat x (lessThanOrEqualTo x)),

     ("(lessThanOrEqualTo x) describes itself as \"less than or equal to x\"",
      \(x, _) -> expectThat (lessThanOrEqualTo x) (describesItselfAs ("less than or equal to " ++ show x))),
     ("(lessThanOrEqualTo x) describes a mismatch for y as \"y\"",
      \(x, y) -> expectThat (lessThanOrEqualTo x) (describesAMismatchFor y (show y)))]

equalToIgnoringCaseTests :: [(String, (String, String) -> Property)]
equalToIgnoringCaseTests =
    [("\"ABCdef\" is equal to \"AbCdEf\" ignoring case",
      \(x, _) -> expectThat x (equalToIgnoringCase (oscillatingCase x))),
     ("\"ABCdef\" is equal to \"abcdef\" ignoring case",
      \(x, _) -> expectThat x (equalToIgnoringCase (map toLower x))),
     ("\"ABCdef\" is equal to \"ABCDEF\" ignoring case",
      \(x, _) -> expectThat x (equalToIgnoringCase (map toUpper x))),
     ("\"ABCdef\" is not equal to \"ABCdefGHI\"",
      \(x, y) -> length x /= length y ==> expectThat x (not_ (equalToIgnoringCase y))),

     ("(equalToIgnoringCase x) describes itself as \"x ignoring case\"",
      \(x, _) -> expectThat (equalToIgnoringCase x) (describesItselfAs (show x ++ " ignoring case"))),
     ("(equalToIgnoringCase x) describes a mismatch for y as \"y\"",
      \(x, y) -> expectThat (equalToIgnoringCase x) (describesAMismatchFor y (show y)))]
    where
    oscillatingCase string = oscillatingCase' True string
    oscillatingCase' _ "" = ""
    oscillatingCase' True (c : cs) = (toUpper c) : oscillatingCase' False cs
    oscillatingCase' False (c : cs) = (toLower c) : oscillatingCase' True cs

tests :: (Show a, Arbitrary a) => [(String, (a, a) -> Property)] -> IO [Result]
tests cases = do
    mapM quickCheckResult $ map (uncurry label) cases

hamcrestTests = (liftM concat) $ sequence
    [tests isTests,
     tests equalToTests,
     tests greaterThanTests,
     tests lessThanTests,
     tests greaterThanOrEqualToTests,
     tests lessThanOrEqualToTests,
     tests equalToIgnoringCaseTests]
