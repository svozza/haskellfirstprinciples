import Test.QuickCheck
import Fool

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

foolGenProb :: Gen Fool
foolGenProb = frequency [ (1, return Frue)
                        , (2, return Fulse)]

main :: IO ()
main = do
  sample foolGenProb 
