{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Control.Monad (join)
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

-- Another exercise

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

-- Demo

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog = Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers = Person
       (HumanName "Big Bird")
       (DogName "Barkley")
       (Address "Sesame Street")

chris :: Person
chris = Person
        (HumanName "Chris Allen")
        (DogName "Papu")
        (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- Exercise

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader a) where
  fmap f (Reader x) =
    Reader $ f . x

instance Applicative (Reader r) where
  pure a = Reader (\r -> a)
  Reader f <*> Reader g =
      Reader (\r -> f r (g r))

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    join $ Reader $ \r -> aRb (ra r)

getDogR'' :: Reader Person Dog
getDogR'' = Dog <$> Reader dogName <*> Reader address

-- Monad of functions

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind m k = \r -> k (m r) r

getDogRm :: Person -> Dog
getDogRm = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Reader (Dog <$> dogName <*> address)
