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
countTheBeforeVowel x = countTheBeforeVowel' . onlyJust $ map justWords $ splitWords x

-----------------------------------

-- 3. Return the number of letters that are vowels in a word

-- Creates Just for words

justVowels :: Char -> Maybe Char
justVowels c = if c `elem` "aeiou" then Just c else Nothing

countVowels :: String -> Integer
countVowels x = toInteger . length . onlyJust $ map justVowels x

---------------------------------------

-- Validate the word

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou" -- not used, had it in previous exercise

mkWord :: String -> Maybe Word'
mkWord x = if countVowels x > toInteger (length x) - countVowels x
           then Nothing
           else Just $ Word' x

--------------------------------------

-- It's only natural

data Nat = Zero
         | Succ Nat
           deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger x =
    case x of
      Zero -> 0
      Succ n -> succ $ natToInteger n

integerToNat' :: Integer -> Nat
integerToNat' x = if x == 0
                  then Zero
                  else Succ $ integerToNat' $ pred x

integerToNat :: Integer -> Maybe Nat
integerToNat x = if x < 0
                 then Nothing
                 else Just $ integerToNat' x

-----------------------------------------

-- Small library for Maybe

-- 1. Boolean tests

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2. Maybe catamorphism

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee fallback f m =
    case m of
      Nothing -> fallback
      Just x -> f x

-- 3. Fallback value

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

-- 4. Convert between List and Maybe

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5. Drop Nothings

catMaybes :: [Maybe a] -> [a]
catMaybes = onlyJust

-- 6. "sequence"

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe list =
    go list []
      where
        go [] acc = Just $ reverse acc
        go (x:xs) acc =
          case x of
            Just y -> go xs (y:acc)
            Nothing -> Nothing

-----------------------------------

-- Small library for Either

-- 1. Use foldr

left' :: Either a b -> Maybe a
left' (Left a) = Just a
left' (Right _) = Nothing

lefts'' :: [Either a b] -> [Maybe a]
lefts'' = foldr (\ab acc -> acc ++ [left' ab]) []

lefts' :: [Either a b] -> [a]
lefts' = catMaybes . lefts''

-- 2. Use foldr

right' :: Either a b -> Maybe b
right' (Left _) = Nothing
right' (Right a) = Just a

rights'' :: [Either a b] -> [Maybe b]
rights'' = foldr (\ab acc -> acc ++ [right' ab]) []

rights' :: [Either a b] -> [b]
rights' = catMaybes . rights''

-- 3. Partition Eithers

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4. eitherMaybe

-- eitherMaybe :: (b -> c) -> Either a b -> Maybe c
