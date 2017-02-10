module Jammin where

import Data.List

data Fruit = Peach | Plum | Apple | Blackberry
  deriving (Eq, Show, Ord)

data JamJars = 
  Jam { fruit :: Fruit
      , jars  :: Int }
      deriving (Eq, Show)

instance Ord JamJars where
  compare a b = compare (jars a) (jars b) 

row1 = Jam Apple 4
row2 = Jam Plum 7
row3 = Jam Blackberry 10
row4 = Jam Peach 1
row5 = Jam Plum 4
row6 = Jam Apple 12

allJam = [row1, row2, row3, row4, row5, row6]

jamCount = map jars allJam

totalJam :: [JamJars] -> Int
totalJam = sum . map jars

mostRow :: [JamJars] -> JamJars
mostRow = maximum

compareKind (Jam k _) (Jam k' _) = compare k k'

sortByKind :: [JamJars] -> [JamJars]
sortByKind = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\a b -> fruit a == fruit b) . sortByKind
