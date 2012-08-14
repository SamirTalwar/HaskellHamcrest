import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit

import Hamcrest.Matchers.EqualToTest

main = do
    results <- mapM quickCheckResult equalToTests
    if all isSuccess results then exitWith ExitSuccess else exitWith (ExitFailure 1)
