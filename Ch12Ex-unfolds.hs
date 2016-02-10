
import Data.List
import Data.Maybe

-- Unfolds

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n + x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = go (n * x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
    where go :: [a] -> [[a]] -> [a]
          go xs' [] = xs'
          go xs' (x:xs) = go (xs' ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

-----------------------------------------

-- Write your own iterate and unfoldr

-- 1. Write myIterate using direct recursion

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

-- OMG I MADE IT WORK -> No need for the [],
-- it will never be evaluated.

-- 2. Write myUnfoldr using direct recursion

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
    case (f x) of
      Just (a, b) -> a : myUnfoldr f b
      Nothing -> []

-- IN YOUR FACE

-- 3. Stuff that. Rewrite myIterate using unfoldr

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = unfoldr (\x -> Just (x, f x))
