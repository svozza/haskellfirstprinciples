myWords :: String -> [String]
myWords "" = []
myWords s  = takeWhile ((/=) ' ') s : myWords (dropWhile ((==) ' ') $ dropWhile ((/=) ' ') s)

myLines :: String -> [String]
myLines ""   = []
myLines s    = takeWhile ((/=) '\n') s : myLines (dropWhile ((==) '\n') $ dropWhile ((/=) '\n') s)

splitOn :: Char -> String -> [String]
splitOn c s
  | s == ""    = []
  | otherwise  = takeWhile ((/=) c) s : myLines (dropWhile ((==) c) $ dropWhile ((/=) c) s)

splitOnSpace = splitOn ' '

splitOnNewLine = splitOn '\n'
