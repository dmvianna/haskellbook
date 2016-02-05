module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = ["zero",
                 "one",
                 "two",
                 "three",
                 "four",
                 "five",
                 "six",
                 "seven",
                 "eight",
                 "nine"] !! n
                                                          

digits :: Int -> [Int]
digits n = go n []
    where go x xs
              | div x 10 == 0 = mod x 10 : xs
              | otherwise = go (div x 10) (mod x 10 : xs)

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

myWords :: String -> Char -> [String]
myWords s c = go s []
    where drops = dropWhile (/= c)
          takes = takeWhile (/= c)
          go x xs
              | null x = xs
              | head x == c = go (drop 1 x) xs
              | otherwise = go (drops x)
                            (xs ++ [takes x])
                                    
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

inList :: Eq a => a -> [a] -> Bool
inList x xs
    | null xs = False
    | otherwise = (x == head xs) || (inList x $ tail xs)

noArt :: String -> [String]
noArt s = go (words s) []
    where go xs acc
              | null xs = acc
              | not $ (head xs) `inList` ["the","a","an"] =
                  go (tail xs) (acc ++ [head xs])
              | otherwise = go (tail xs) acc

myZip :: [a] -> [b] -> [(a, b)]
myZip xs ys = go xs ys []
    where go x y acc
            | null x || null y = acc
            | otherwise = go (tail x) (tail y) (acc ++ [(head x, head y)])
                   
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = go xs ys []
    where go x y acc
              | null x || null y = acc
              | otherwise = go (tail x) (tail y) (acc ++ [f (head x) (head y)])
