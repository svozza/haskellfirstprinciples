module Lib where

half :: Fractional a => a -> a
half x = x / 2

f :: Eq a => Int -> [a] -> Int
f n xs = length (take n xs)

square :: Num a => a -> a
square x = x * x
