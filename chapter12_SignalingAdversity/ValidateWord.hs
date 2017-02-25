import Data.Char
import Data.Ord

newtype Word' = Word' String
  deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = (flip elem $ vowels) . toLower

isConsonant :: Char -> Bool
isConsonant = (flip elem $ consonants) . toLower
  where consonants = filter (not . isVowel) ['a'..'z']

mkWord :: String -> Maybe Word'
mkWord s = case comparing length vowels consonants of
  GT -> Nothing
  _  -> Just (Word' s)
  where vowels     = filter isVowel s
        consonants = filter isConsonant s
