add' :: (Eq a, Num a) => a -> a
add' 0 = 0
add' n = n + (add' (n - 1))

mult' :: (Integral a) => a -> a -> a
mult' multiplicand multiplier = go (abs multiplicand) multiplier 0
  where go md m result
         | md == 0   = if multiplicand < 0 then (negate result) else result
         | otherwise = go (md - 1) m (result + m)

data DividedResult a = Result a | DividedByZero
  deriving (Show, Eq)

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' num denom  = go (abs num) (abs denom) 0
  where go n d count
         | n < d     = if isOneNegative num denom then ((negate count), n) else (count, n)
         | otherwise = go (n - d) d (count + 1)
        isOneNegative a b
         | a < 0 && b > 0 = True
         | a > 0 && b < 0 = True
         | otherwise      = False

dividedBy :: Integer -> Integer -> DividedResult Integer
dividedBy _ 0 = DividedByZero
dividedBy num denom = case (dividedBy' num denom) of
                           (x, y) -> Result x
