module Lib where

import Data.Char (toUpper)

half :: Fractional a => a -> a
half x = x / 2

f :: Eq a => Int -> [a] -> Int
f n xs = length (take n xs)

square :: Num a => a -> a
square x = x * x

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (s:ss) = (toUpper s):ss
