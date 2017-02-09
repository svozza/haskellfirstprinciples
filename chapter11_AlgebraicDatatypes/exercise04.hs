{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Goats, Goats) where
  tooMany (Goats n, Goats m) = (n + m) > 42

instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (a, b) = tooMany (a + b)
