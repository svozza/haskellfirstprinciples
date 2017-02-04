import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 1009
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\x acc ->
                       case x of
                         DbDate d -> d:acc
                         _        -> acc) 
                     []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\x acc ->
                         case x of
                           DbNumber n -> n:acc
                           _          -> acc)
                       []

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (\x acc ->
                case x of
                  DbNumber n -> n + acc
                  _          -> acc)
              0

avgDb :: [DatabaseItem] -> Double
avgDb xs = sum / count
  where sum   = fromIntegral $ sumDb xs
        count = fromIntegral $ length $ filterDbNumber xs

avgDb' :: [DatabaseItem] -> Double
avgDb' xs = case avg xs of
  (x, y) -> (fromIntegral x) / (fromIntegral y)
  where avg = foldr (\x (acc, count) ->
                      case x of
                        DbNumber n -> (acc + n, count + 1)
                        _          -> (acc, count))
                    (0, 0) 
