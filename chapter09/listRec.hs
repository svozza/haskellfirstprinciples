take' :: Int -> [a] -> [a]
take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (x:xs) = drop' (n - 1) xs

splitAt'' :: Int -> ([a], [a]) -> ([a], [a])
splitAt'' _ (xs, []) = (xs, [])
splitAt'' 0 (xs, ys) = (reverse xs, ys)
splitAt'' n (xs, y:ys) = splitAt'' (n - 1) (y:xs, ys)

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = splitAt'' n ([], xs)

