module Cipher (caesar, uncaesar, toAlphaNum)
  where

import Data.Char

toAlphaNum c
  | isUpper c = (ord c) - 65
  | otherwise = (ord c) - 97

toAscii c n
  | isUpper c = chr (n + 65)
  | otherwise = chr (n + 97)

shiftR :: Int -> Int -> Int
shiftR shift n
  | (shift + n) >= 26 = (shift + n) - 26
  | otherwise         = shift + n

shiftL :: Int -> Int -> Int
shiftL shift n
  | (n - shift) < 0 = (n - shift) + 26
  | otherwise       = n - shift

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift shifter n c = ((toAscii c) . shifter n . toAlphaNum) c

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAscii c && isLetter c

shiftLetter :: (Int -> Int -> Int) -> Int -> Char -> Char
shiftLetter shifter n c
  | isAsciiLetter c = shift shifter n c
  | otherwise       = c

shiftLetterR = shiftLetter shiftR
shiftLetterL = shiftLetter shiftL

caesar :: Int -> String -> String
caesar n = map $ shiftLetterR n

uncaesar :: Int -> String -> String
uncaesar n = map $ shiftLetterL n
