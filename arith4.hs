module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print (roundTrip 4 :: Int)
  print (id 4)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sums :: (Eq a, Ord a, Num a) => a -> a
sums 1 = 1
sums n = if n > 0
         then n + sums (n - 1)
         else n + sums (n + 1)

data DividedResult =
    Result (Integer, Integer)
        | DividedByZero
    deriving Show
                 
dividedBy :: Integer -> Integer -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom = go num denom 0
    where go n d count
              | n < d = Result (count, n)
              | otherwise = go (n - d) d (count + 1)

divBy :: Integer -> Integer -> DividedResult
divBy num 0 = DividedByZero
divBy num denom = go (abs num) (abs denom) 0
    where go n d count
              | n < d && num > 0 && denom > 0 = Result (count, n)
              | n < d && num < 0 && denom > 0 = Result (negate count, negate n)
              | n < d && num < 0 && denom < 0 = Result (count, negate n)
              | n < d && num > 0 && denom < 0 = Result (negate count, n)
              | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 a
    | a > 100 = a - 10
    | otherwise = mc91 $ mc91 $ a + 11
