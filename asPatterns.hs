import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf sub par =
    case sub of
      [] -> True
      x:xs -> x `elem` par && isSubsequenceOf xs par

-- isLetter :: Char -> Bool
-- isLetter = flip elem (['a'..'z'] ++ ['A'..'Z'])

splitWords :: String -> [String]
splitWords str =
    go str [] True
        where
          go [] acc _ = reverse acc
          go (x:xs) [] True
              | isLetter x = go xs [[x]] False
              | otherwise = go xs [] True
          go (x:xs) acc@(w:ws) startWord
              | (not $ isLetter x) && startWord = go xs acc startWord
              | (isLetter x && startWord) = go xs ([x]:acc) False
              | (not $ isLetter x) && not startWord = go xs acc True
              | (isLetter x) && not startWord = go xs ((w ++ [x]):ws) startWord


capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map (\x -> (x, capitalizeWord x)) $ splitWords str

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x:xs

capitalizeParagraph :: String -> String
capitalizeParagraph s =
    go s [] True
        where
          go [] acc _ = acc
          go (x:xs) acc period
              | x == '.' = go xs (acc ++ [x]) True
              | x `elem` ['a'..'z'] && period =
                go xs (acc ++ [toUpper x]) False
              | x `elem` ['A'..'Z'] && period =
                go xs (acc ++ [x]) False
              | otherwise = go xs (acc ++ [x]) period

-- Phone excercise

data DaPhone = Sommat

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses = [1..4]
type Presses = Int

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined
