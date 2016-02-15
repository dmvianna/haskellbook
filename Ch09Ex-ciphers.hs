module Cipher where

import Data.Char


charToRight :: Int -> Char -> Char
charToRight n x
    | ord x >= 97 && ord x <= 122 = go n x ['a'..'z'] 97
    | ord x >= 65 && ord x <= 90 = go n x ['A'..'Z'] 65
    | otherwise = x
    where go nn xx xxs off
              | ord xx - off + nn < 0 = go nn xx xxs (off - 26)
              | ord xx - off + nn > 25 = go nn xx xxs (off + 26)
              | otherwise = xxs !! (ord xx - off + nn)

inCaesar :: Int -> String -> String
inCaesar n = map (charToRight n)

unCaesar :: Int -> String -> String
unCaesar n = map (charToRight $ negate n)

vigenere :: (Int -> Int) -> String -> String -> String
vigenere switch baseKeys baseExs =
  case (baseKeys, baseExs) of
    (_, "") -> ""
    ("", xs) -> xs
    (bks, bxs) ->
      go bks bxs []
          where
            go keys exs acc =
                case (keys, exs) of
                  (_, []) -> acc
                  ([], xs) -> go baseKeys xs acc
                  (k:ks, x:xs) -> goAgain k ks x xs
                      where goAgain k ks x xs
                                | x `elem` ['A'..'Z'] =
                                    go ks xs (acc
                                              ++ [charToRight
                                                  (switch (ord k - 65)) x])
                                | x `elem` ['a'..'z'] =
                                    go ks xs (acc
                                              ++ [charToRight
                                                  (switch (ord k - 97)) x])
                                | otherwise =
                                    go keys xs (acc ++ [x])

inVig :: String -> String -> String
inVig = vigenere id

unVig :: String -> String -> String
unVig = vigenere negate

-- unVig "ally" $ inVig "ally" "meet at dawn"
