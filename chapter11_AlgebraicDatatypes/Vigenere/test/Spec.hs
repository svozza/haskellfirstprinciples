import Test.QuickCheck
import Vigenere (vigenere, unvigenere)

prop_vigenere :: String -> String -> Bool
prop_vigenere kw s =
  s == unvigenere kw (vigenere kw s)

main :: IO ()
main = do
  quickCheck prop_vigenere
