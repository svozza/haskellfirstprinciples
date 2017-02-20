import Data.Char

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' sub ys = sub == (fst $ tuple ys)
  where tuple = foldr (\x (s, y:ys) ->  
                  if x == y
                  then (x:s, ys)
                  else (s, y:ys)) 
                ([], (reverse sub))

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalize . words 
  where capitalize s@(x:xs) = (s, (toUpper x):xs)
