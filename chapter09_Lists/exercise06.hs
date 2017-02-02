import Data.Bool

threeNeg :: Num a => [a] -> [a]
threeNeg = map (\x -> bool x (negate x) (x == 3))
