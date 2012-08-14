module Hamcrest.Matchers.Not where

import qualified Prelude

not f x = Prelude.not (f x)
