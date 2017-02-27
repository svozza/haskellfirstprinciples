isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x maybe = mayybee x id maybe

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs  = Just $ head xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just x) -> x) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case filter isNothing xs of
  [] -> foldr (\(Just x) (Just xs) -> Just (x:xs)) (Just []) xs
  _  -> Nothing

