import System.Exit

import Test.QuickCheck.Test

import Hamcrest.MatchersTests

main = do
    results <- hamcrestTests
    if all isSuccess results then exitWith ExitSuccess else exitWith (ExitFailure 1)
