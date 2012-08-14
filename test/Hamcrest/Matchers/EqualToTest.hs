module Hamcrest.Matchers.EqualToTest where

import Hamcrest.Matchers (equalTo, not_)
import Hamcrest.Assertions (expectThat)

equalToTests = [\x -> expectThat x (equalTo x),
                \x -> expectThat x (not_ (equalTo (x + 1)))]
