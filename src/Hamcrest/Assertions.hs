module Hamcrest.Assertions where

import Hamcrest

expectThat actual matcher = matcher `matches` actual
