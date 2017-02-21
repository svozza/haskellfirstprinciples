import Data.Char
import Data.List

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' sub ys = sub == (fst $ tuple ys)
  where tuple = foldr (\x (z, ys@(y:ys')) ->  
                  if x == y
                  then (x:z, ys')
                  else (z, ys)) 
                ([], (reverse sub))

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalize . words 
  where capitalize s@(x:xs) = (s, (toUpper x):xs)

capitalizeWord :: String -> String
capitalizeWord (s:ss) = (toUpper s):ss

splitOn :: Char -> String -> [String]
splitOn c s = go c (reverse s) []
  where go _ [] xs    = xs
        go c (s:ss) []
          | c == s    = go c ss ([]:[])
          | otherwise = go c ss ([s]:[])
        go c (s:ss) (xs:xss)
          | c == s    = go c ss ([]:xs:xss)
          | otherwise = go c ss ((s:xs):xss)

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate ". " . map (capitalizeWord . dropWhile isSpace) . splitOn '.'
