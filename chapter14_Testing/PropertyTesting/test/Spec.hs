import Test.QuickCheck
import Test.QuickCheck.Function
import Text.Show.Functions

import Data.List (sort)

import Lib (half, f)

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

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z =
  x `f` (y `f` z) == (x `f` y) `f` z

commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutative f x y =
  x `f` y == y `f` x

plusAssociative = associative (+)

plusCommutative = commutative (+)

multAssociative = associative (*)

multCommutative = commutative (*)

nonZeroInt :: Gen Int
nonZeroInt = suchThat arbitrary (/=0)

prop_quotRem :: Integral a => a -> a -> Bool
prop_quotRem x y =
  ((quot x y) * y) + (rem x y) == x

prop_divMod :: Integral a => a -> a -> Bool
prop_divMod x y =
  ((div x y) * y) + (mod x y) == x

powCommutative  = commutative (^)

powAssociative  = associative (^)

prop_reverse :: Eq a => [a] -> Bool
prop_reverse xs = reverse (reverse xs) == id xs

prop_dollar :: (Eq a, Eq b) => (a -> b) -> a -> Bool
prop_dollar f a = (f $ a) == f a

prop_compose :: (Eq a, Eq b, Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_compose f g x =
  (f . g) x == f (g x)

prop_plusplus :: Eq a => [a] -> [a] -> Bool
prop_plusplus xs ys =
  foldr (:) xs ys == (++) xs ys

prop_concat :: Eq a => [[a]] -> Bool
prop_concat xss =  
  foldr (++) [] xss == concat xss

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
  quickCheck (forAll nonZeroInt $ \x -> forAll nonZeroInt $ \y -> prop_quotRem x y)
  quickCheck (forAll nonZeroInt $ \x -> forAll nonZeroInt $ \y -> prop_divMod x y)
  quickCheck (prop_reverse :: String -> Bool)
  quickCheck (prop_dollar :: (Int -> Char) -> Int -> Bool)
  quickCheck (prop_compose :: (Char -> String) -> (Int -> Char) -> Int -> Bool)
  --quickCheck (prop_plusplus :: [Int] -> [Int] -> Bool) Not equivalent
  quickCheck (prop_concat :: [String] -> Bool)
