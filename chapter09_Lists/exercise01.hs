eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False False = [False]
eftBool True True = [True]
eftBool False _ = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = x : (eftOrd (succ x) y)  

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = a : (eftInt (a + 1) b)

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = x : (eftChar (succ x) y)
