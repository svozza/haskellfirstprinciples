myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []      = False
myAny p (x:xs)
  | p x == True = True
  | otherwise   = myAny p xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
  | x == a    = True
  | otherwise = myElem a xs

myElem' a = myAny $ (==) a

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

orderingBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
orderingBy _ _ [x] = x
orderingBy ord f (x:xs) = go x xs
  where
    go x []     = x
    go x (y:ys)
      | f x y == ord = go x ys
      | otherwise   = go y ys 

myMaximumBy = orderingBy GT 

myMinimumBy = orderingBy LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
  
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
