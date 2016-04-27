
import Control.Applicative
import Data.Char

-- Demo

hurr :: Num a => a -> a
hurr = (*2)

durr :: Num a => a -> a
durr = (+10)

m :: Num a => a -> a
m = hurr . durr

m' :: Num a => a -> a
m' = fmap hurr durr

m2 :: Num a => a -> a
m2 = (+) <$> hurr <*> durr

m3 :: Num a => a -> a
m3 = liftA2 (+) hurr durr

hurrDurr :: Num a => a -> a
hurrDurr = do
  a <- hurr
  b <- durr
  return (a + b)

-- Short exercise

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
  a <- cap
  b <- rev
  return (a, b)

tupled''' :: [Char] -> ([Char], [Char])
tupled''' xs = fmap rev $ (cap >>= (,)) (xs)
