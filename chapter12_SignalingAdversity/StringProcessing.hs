import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

not' :: String -> String -> Bool
not' _ "" = False
not' s t  = s /= (take len t) || not' s (drop len t)
  where len = length s

notThe' :: String -> Maybe String
notThe' s
  | not' "the" s == True = Just s
  | otherwise            = Nothing

notThe :: String -> Maybe String
notThe s = bool (Just s) Nothing (s == "the")

replaceThe :: String -> String
replaceThe = intercalate " " . go . words
  where go [] = []  
        go (xs:xss) = case notThe xs of        
          Nothing -> "a":(go xss)
          Just _  -> xs:(go xss)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go ss@(_:xs)
         | length ss < 2 = 0
         | otherwise     = case take 2 ss of
           [s, t:_] -> case (notThe s, isVowel t) of
             (Nothing, Just _) -> 1 + go xs
             _                 -> 0 + go xs
                    

isVowel :: Char -> Maybe Char
isVowel c = bool Nothing (Just c) (c `elem` (vowels ++ (map toUpper vowels)))
  where vowels = "aeiou"

getVowels :: String -> String
getVowels ""     = ""
getVowels (x:xs) = case isVowel x of
  Just s  -> s:(getVowels xs)
  Nothing -> getVowels xs

getVowels' = catMaybes . map isVowel

countVowels :: String -> Integer 
countVowels = fromIntegral . length . getVowels
