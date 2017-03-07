type Name = String
type Age  = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknown $
                                   "Name was: " ++ show name ++
                                   " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter a name."
  name <- getLine
  putStrLn "Please enter their age."
  age <- getLine
  case mkPerson name (read age) of
    Right person -> do
      putStrLn $ "Yay! Successfully got a person: " ++ (show person)
    Left error   -> do
      putStrLn $ "Ooops. An error occured: " ++ (show error)
