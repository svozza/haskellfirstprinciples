module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d     = (count, n) 
         | otherwise = go (n - d) d (count + 1)

-- used type signature from Chapter 8 as one given in this
-- chapter significantly complicates the implementation
multipliedBy :: Integral a => a -> a -> a
multipliedBy multiplicand multiplier 
  | sigs == 0   = negate result
  | otherwise   = result
  where sigs         = (signum multiplicand) + (signum multiplier)
        go md mr total
         | md == 0   = total
         | otherwise = go (md - 1) mr (total + mr)
        result       = go (abs multiplicand) (abs multiplier) 0

oneThroughThree :: Gen Int
oneThroughThree =elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => 
               Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` (4 :: Int)
  
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4,2)

  describe "Multiplication" $ do
    it "5 multiplied by 3 is 15" $ do
      multipliedBy 5 3 `shouldBe` 15
    it "5 multiplied by -3 is -15" $ do
      multipliedBy 5 (-3) `shouldBe` (-15)
    it "-5 multiplied by 3 is -15" $ do
      multipliedBy (-5) 3 `shouldBe` (-15)
    it "-5 multiplied by -3 is 15" $ do
      multipliedBy (-5) (-3) `shouldBe` 15

  describe "Property tests" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
