import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalise :: String -> String
capitalise ""     = ""
capitalise (s:ss) = toUpper s : ss

capsLock :: String -> String
capsLock ""     = ""
capsLock (s:ss) = toUpper s : capsLock ss

capFirst :: String -> Char
capFirst = head . capitalise
