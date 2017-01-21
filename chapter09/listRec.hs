take' :: Int -> [a] -> [a]
take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (x:xs) = drop' (n - 1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' num xs
  | num > length xs = (xs, [])
  | otherwise       = go num ([], xs)
  where
    go _ ([], []) = ([], [])
    go 0 (ys, zs) = (reverse ys, zs)
    go n (ys, z:zs) = go (n - 1) (z:ys, zs)
