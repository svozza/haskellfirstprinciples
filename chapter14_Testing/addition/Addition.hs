module Addition where

import Test.Hspec

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
