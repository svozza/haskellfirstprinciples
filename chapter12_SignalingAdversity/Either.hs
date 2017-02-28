lefts' :: [Either a b] -> [a]
lefts' = foldr (\either xs -> case either of
                 Left x -> x:xs
                 _      -> xs) 
               []

rights' :: [Either a b] -> [b]
rights' = foldr (\either xs -> case either of
                 Right x -> x:xs
                 _       -> xs)
               []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _)  = Nothing
eitherMaybe' f (Right x) = Just $ f x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)
