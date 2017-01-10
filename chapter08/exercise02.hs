add' :: (Eq a, Num a) => a -> a
add' 0 = 0
add' n = n + (add' (n - 1))

mult' :: (Integral a) => a -> a -> a
mult' multiplicand multiplier = go (abs multiplicand) multiplier 0
  where go md m result
         | md == 0   = if multiplicand < 0 then (negate result) else result
         | otherwise = go (md - 1) m (result + m)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | num > 0 && n < d = (count, n)
         | num < 0 && n > d = (count, n)
         | num < 0 && n < d = go (n + d) d (count + 1)
         | otherwise        = go (n - d) d (count + 1)
