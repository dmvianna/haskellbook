import Data.Char

fUp :: String -> String
fUp a = filter isUpper a

title :: String -> String
title [] = []
title (x:xs) = toUpper x : title xs

hUp :: String -> Char
hUp (x:xs) = toUpper x

hUp2 :: String -> Char
hUp2 = toUpper . head

hUp3 :: String -> String
hUp3 = map toUpper

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x (y:[]) = x == y
myElem x (y:ys) = myElem x [y] || myElem x ys

myReverse :: [a] -> [a]
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish (x:[]) = x
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f (x:[]) = f x
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f' (x:xs) = go f' x xs
    where
      go f x (y:[])
          | f x y == GT = x
          | f x y == EQ = x
          | f x y == LT = y
      go f x (y:ys) = go f (go f x [y]) ys

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:y:[])
    | f x y == LT = x
    | f x y == EQ = x
    | f x y == GT = y
myMinimumBy f (x:x':xs) = myMinimumBy f $ (myMinimumBy f (x:x':[])):xs

myCompare :: Ord a => [a] -> a
myCompare (x:[]) = x
--myCompare (x:x':[]) = max x x'
myCompare (x:x':xs) = myCompare $ max x x':xs
          

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare


