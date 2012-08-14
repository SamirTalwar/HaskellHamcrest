module Hamcrest.Matchers.EqualTo where

equalTo :: (Eq a) => a -> a -> Bool
equalTo = (==)
