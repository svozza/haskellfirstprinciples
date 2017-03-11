import Test.QuickCheck

import Data.List (sort)

import Lib

halfIdentity :: Fractional a => a -> a
halfIdentity = (2*) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered = listOrdered . sort

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y =
  x * y == y * x

main :: IO ()
main = do
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck (prop_halfIdentity :: Float -> Bool)
  quickCheck (prop_listOrdered :: [Int] -> Bool)
  quickCheck (prop_listOrdered :: String -> Bool)
  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (plusCommutative :: Int -> Int  -> Bool)
  quickCheck (multAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (multCommutative :: Integer -> Integer -> Bool)
