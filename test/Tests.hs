import System.Exit

import Test.QuickCheck.Test

import Hamcrest.MatchersTests

main = do
    results <- hamcrestTests
    exitWith (if all isSuccess results then ExitSuccess else ExitFailure 1)
