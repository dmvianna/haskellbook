{-# LANGUAGE RankNTypes #-}

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled
                deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = WhatThisIsCalled
  fmap f WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)

data CountingBad a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n) (f a)

-- lmls ~ List (Maybe (List String))

-- lmls ~ [Maybe [[Char]]]
-- [Maybe [[Char]]] -> [Maybe Char]
-- [Maybe [[Char]]] -> [Maybe [Char]]
-- [Maybe [[Char]]] -> [Maybe [[Char]]]

-- Lifting

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (* 2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123"++) (fmap show ioi))
    in fmap (* 3) changed

-- Maybe

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Either

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left  e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left  e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show

-- liftedInc :: (Functor f, Num b) => f b -> f b
-- liftedInc = fmap (+1)

-- liftedShow :: (Functor f, Show a) => f a -> f String
-- liftedShow = fmap show

-- Exercise

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- 16.12

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

-- 16.13

data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (f fa)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (fmap f fa)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- 16.14

getInt :: IO Int
getInt = fmap read getLine

-- getLine abhors emacs
-- meTooIsm :: IO String
-- meTooIsm = do
--   input <- getLine
--   return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

-- 16.15

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- Applicative ZipList'

-- applicative ZipList'
