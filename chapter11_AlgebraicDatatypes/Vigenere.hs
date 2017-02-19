module Vigenere where

import Data.Char
import Data.List
import Cipher as C

toKeyword :: String -> String -> (String, String)
toKeyword kw = foldr (\c (x, y:ys) -> 
                        if isLetter c
                        then (y:x, ys ++ [y])
                        else (c:x, y:ys))
                     ("", (reverse kw))

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
encode f kw s = map f $ cipherTuples kw s

vigenere = encode encrypt

unvigenere = encode decrypt
