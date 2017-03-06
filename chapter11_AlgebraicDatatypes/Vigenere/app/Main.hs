module Main where

import Vigenere as V

main :: IO ()
main = do
  putStrLn "Please input your keyword"
  keyword <- getLine
  putStrLn "Do you wish to encrypt or decrypt?"
  action  <- getLine
  case action of
    "encrypt" -> do 
      putStrLn "Please enter the sentence to encrypt."
      str <- getLine
      putStrLn $ V.vigenere keyword str
    "decrypt" -> do
      putStrLn "Please enter the sentence to decrypt."
      str <- getLine
      putStrLn $ V.unvigenere keyword str
    _         -> do
      putStrLn "You must input the exact command \"encrypt.\" or \"decrypt\""
      main
  return ()
