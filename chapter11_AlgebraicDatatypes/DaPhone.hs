import Data.Char
import Data.List
import Data.Function

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya"]

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

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (+) 0 . map snd

largest :: [[a]] -> [a]
largest = foldr (\x z -> if length x > length z
                         then x
                         else z)
                []

-- This is horrendously unsafe but Maybes are the focus of the next chapter
mostPopularLetter :: String -> Char
mostPopularLetter = head . largest . group . sort . filter isLetter

costliestLetter :: String -> (Digit, Presses)
costliestLetter s = (\(_, p) -> (letter, p)) $ getTaps $ reverseTaps phone letter
  where
    letter                         = mostPopularLetter s
    getTaps [(d, p)]               = (d, p)
    getTaps xs@[('*', p), (d, p')] = (d, fingerTaps xs)


coolestLetter :: [String] -> Char
coolestLetter = mostPopularLetter . concat

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = go c (reverse s) []
  where go _ []     xs       = xs
        go c (s:ss) []       = go c ss (if c == s then [] else [[s]])
        go c (s:ss) (xs:xss) = go c ss (if c == s then []:xs:xss else (s:xs):xss)

-- This is horrendously unsafe but Maybes are the focus of the next chapter
coolestWord :: [String] -> String
coolestWord = head . largest . group . sort . splitOn ' ' . intercalate " "

       
