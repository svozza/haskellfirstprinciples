import Data.Char
import Data.List
import Debug.Trace

type Digit   = Char
type Presses = Int

data DaPhone = DaPhone [(Digit, String)]

phone :: DaPhone
phone = DaPhone [('1', "")
                ,('2', "ABC")
                ,('3', "DEF")
                ,('4', "GHI")
                ,('5', "JKL")
                ,('6', "MNO")
                ,('7', "PQRS")
                ,('8', "TUV")
                ,('9', "WXYZ")
                ,('*', "")
                ,('0', " ")
                ,('#', ".,")]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone xs) c 
  | isUpper c = ('*', 1) : (filtered c xs)
  | otherwise = filtered (toUpper c) xs
    where filtered c = map (getPresses c) . filter (hasChar c)
          hasChar x (d, s)
            | x == d     = True
            | x `elem` s = True
            | otherwise  = False
          getPresses c (d, s) = case elemIndex c s of
            Just x  -> (d, x + 1)
            Nothing -> (d, (length s) + 1)
