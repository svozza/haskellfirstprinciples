data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat' :: Integer -> Nat
integerToNat' 0 = Succ Zero
integerToNat' n = Succ (integerToNat' (n - 1))

integerToNat n
  | n < 0     = Nothing
  | otherwise = Just (integerToNat' n)
