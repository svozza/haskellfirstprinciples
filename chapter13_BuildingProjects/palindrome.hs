import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

sanitise :: String -> String
sanitise = filter isLetter . map toLower

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let sanitised = sanitise line1
  case sanitised == reverse sanitised of
    True  -> putStrLn "It's a palindrome"
    False -> do putStrLn "Nope"
                exitSuccess
