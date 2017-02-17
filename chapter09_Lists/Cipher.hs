module Cipher where

import Data.Char

toAlphaNum = flip (-) 97
toUnicode = (+) 97

shiftR :: Int -> Int -> Int
shiftR shift n
  | (shift + n) >= 26 = (shift + n) - 26
  | otherwise         = shift + n

shiftL :: Int -> Int -> Int
shiftL shift n
  | (n - shift) < 0 = (n - shift) + 26
  | otherwise       = n - shift

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift shifter n = chr . toUnicode . shifter n . toAlphaNum . ord

shiftLetter :: (Int -> Int -> Int) -> Int -> Char -> Char
shiftLetter shifter n c
  | isLetter c = shift shifter n c
  | otherwise  = c

shiftLetterR = shiftLetter shiftR
shiftLetterL = shiftLetter shiftL

caesar :: Int -> String -> String
caesar n = map $ shiftLetterR n . toLower

uncaesar :: Int -> String -> String
uncaesar n = map $ shiftLetterL n . toLower
