module Hamcrest.Matchers where

not_ f x = Prelude.not (f x)

equalTo :: (Eq a) => a -> a -> Bool
equalTo = (==)
