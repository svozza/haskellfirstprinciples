myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x acc -> p x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x acc -> x == e || acc) False

myElem' x = myAny ((==) x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x == True
                              then x : acc
                              else acc)
                   []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) [] 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\x acc -> case f x acc of
                           GT -> x
                           _  -> acc)
                         x
                         xs
                      
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\x acc -> case f x acc of
                           LT -> x
                           _  -> acc)
                         x
                         xs 
