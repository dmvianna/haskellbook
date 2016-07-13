
-- Exercises 2 & 3

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)
import System.IO


palindrome :: IO () -- doesn't like haskell-mode (emacs)
palindrome = forever $ do
  line1 <- getLine
  case (standardise line1 == reverse (standardise line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess


standardise :: String -> String
standardise = (filter isLetter) . (map toLower)

-- Exercise 4

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                     deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                  "Name was: " ++ show name ++
                  " Age was: " ++ show age

----

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter NAME: "
  name <- getLine
  putStr "Enter AGE: "
  age <- getLine
  case mkPerson name (read age) of
    Right (Person name age) -> do
                putStr "Yay! Successfully got a person: "
                putStrLn $ (show name) ++ " " ++ (show age)
    Left error -> do
                putStr "Error: "
                putStrLn (show error)
