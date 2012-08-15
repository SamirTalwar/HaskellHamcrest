module Hamcrest where

data Matcher a = Matcher { describe :: String,
                           describeMismatch :: a -> String,
                           matches :: a -> Bool }
