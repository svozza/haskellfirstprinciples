take' :: Int -> [a] -> [a]
take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (x:xs) = drop' (n - 1) xs

splitAt' :: Eq a => Int -> [a] -> ([a], [a])
splitAt' num xs = go num ([], xs)
  where go n (ys, zs)
          | zs == []  = (ys, zs)
          | n == 0    = (reverse ys, zs)
          | otherwise = go (n - 1) ((head zs):ys, tail zs)
