module Vigenere (vigenere, unvigenere, toKeyword)
  where

import Data.Char
import Data.List
import Cipher as C

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isLetter c

toKeyword :: String -> String -> (String, String)
toKeyword kw = foldr (\c (x, y:ys) -> 
                       if isAsciiLetter c
                       then (x ++ [y], ys ++ [y])
                       else (x ++ [c], y:ys))
                     ("", kw) 

toTuple :: String -> String -> [(Char, Char)]
toTuple kw s = zip s $ fst $ toKeyword kw s  

shiftMapping :: (Char, Char) -> (Char, Int)
shiftMapping (c, d) = (c, C.toAlphaNum d)

cipherTuples :: String -> String -> [(Char, Int)]
cipherTuples kw s = map shiftMapping $ toTuple kw s

encodeLetter :: (Int -> String -> String) -> (Char, Int) -> Char
encodeLetter f (c, n) = head $ f n [c]

encrypt = encodeLetter C.caesar

decrypt = encodeLetter C.uncaesar

encode :: ((Char, Int) -> Char) -> String -> String -> String
encode _ "" s = s
encode _ _ "" = "" 
encode f kw s = map f $ cipherTuples kw s

vigenere = encode encrypt . filter isAsciiLetter

unvigenere = encode decrypt . filter isAsciiLetter
