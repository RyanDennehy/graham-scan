module Utility where

approxEqual :: (Floating a, Ord a) => a -> a -> Bool
approxEqual a b = (abs (a - b)) < 10e-6
