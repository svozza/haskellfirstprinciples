multiplesOfThree :: Integral a => [a] -> [a]
multiplesOfThree = filter (\x -> (rem x 3) == 0)

numOfMultiplesOfThree :: Integral a => [a] -> Int
numOfMultiplesOfThree = length . multiplesOfThree

myFilter :: String -> [String]
myFilter = filter isArticle . words
  where isArticle word 
          | word == "a"   = False
          | word == "an"  = False
          | word == "the" = False
          | otherwise     = True
