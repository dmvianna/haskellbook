module EqCaseGuard where

import Data.Char

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                     deriving (Eq, Show)

-- mkPerson :: Name
--          -> Age
--          -> Either PersonInvalid Person

-- mkPerson name age
--     | name /= "" && age >= 0 = Right $ Person name age
--     | name == "" = Left NameEmpty
--     | otherwise = Left AgeTooLow


type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
           -> ValidatePerson Age
           -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) =
    Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =
    Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

-- Chapter Exercises

-- Determine the kind
-- k: a = *
-- k: a = *
-- k: f = * -> *

-- String Processing

--------------------------------------

-- 1. the / a

splitWords :: String -> [String]
splitWords string =
    go string []
        where
          go [] acc = reverse acc
          go (x:xs) [] = go xs [[x]]
          go (x:xs) ht@(str@(y:_):ts) =
              if isLetter x == isLetter y
              then go xs ((str ++ [x]):ts)
              else go xs ([x]:ht)

-- theRep :: String -> String
-- theRep x = if x == "the" then "a" else x

-- theA :: String -> String
-- theA string = concatMap theRep $ splitWords string

notThe :: String -> Maybe String
notThe x = if x == "the" then Nothing else Just x

nothA :: Maybe String -> String
nothA x = case x of
  Nothing -> "a"
  Just x -> x

replaceThe :: String -> String
replaceThe x = concatMap (nothA . notThe) $ splitWords x

------------------------------------------------

-- 2. count "the" before vowel

-- Creates Just for words
justWords :: String -> Maybe String
justWords w@(c:_) = if isLetter c then Just w else Nothing

-- Filters Nothing out
onlyJust :: [Maybe a] -> [a]
onlyJust [] = []
onlyJust list =
    go list []
      where
        go [] acc = reverse acc
        go (x:xs) acc =
          case x of
            Just x -> go xs (x:acc)
            Nothing -> go xs acc


-- Counts "the" before vowels (only words present in list)
countTheBeforeVowel' :: [String] -> Int
countTheBeforeVowel' strings =
    go strings 0
        where
          go (_:[]) acc = acc
          go (aWord:bWord@(ch:_):tWords) acc =
              if aWord == "the" && ch `elem` "aeiou"
              then go (bWord:tWords) (acc + 1)
              else go (bWord:tWords) acc



countTheBeforeVowel :: String -> Int
countTheBeforeVowel x = countTheBeforeVowel' $ onlyJust $ map justWords $ splitWords x

-----------------------------------

-- 3. Return the number of letters that are vowels in a word
