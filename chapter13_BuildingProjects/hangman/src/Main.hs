module Main where

import Data.Char
import Hangman (randomWord', freshPuzzle, runGame)

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
