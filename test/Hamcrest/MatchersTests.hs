module Hamcrest.MatchersTests (hamcrestTests) where

import Hamcrest.Matchers
import Hamcrest.Assertions (expectThat)

equalToTests = [\x -> expectThat x (equalTo x),
                \x -> expectThat x (not_ (equalTo (x + 0)))]

hamcrestTests = equalToTests
