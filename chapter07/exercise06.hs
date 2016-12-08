tensDigit :: Integral a => a -> a
tensDigit x = case x `divMod` 10 of
  (x, _) -> snd $ x `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = case x `divMod` 100 of
  (x, _) -> snd $ x `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
  True  -> x
  False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | bool == True = x
  | otherwise    = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = ((f x), y)
