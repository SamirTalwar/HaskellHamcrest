import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit

import Hamcrest.MatchersTests

main = do
    results <- mapM quickCheckResult hamcrestTests
    if all isSuccess results then exitWith ExitSuccess else exitWith (ExitFailure 1)
