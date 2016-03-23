
module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

-- List Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend = append

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f fs <*> xs = append (flatMap (fmap f) (Cons xs Nil)) (fs <*> xs)

-- flatMap (fmap (+1)) $ Cons (Cons 4 Nil) (Cons (Cons 5 Nil) Nil)
-- Cons 5 (Cons 6 Nil)

-- let functions = Cons (+1) (Cons (*2) Nil)
-- let values = Cons 1 (Cons 2 Nil)
-- functions <*> values
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- ZipList Applicative

take' :: Int -> List a -> List a
take' = undefined

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = undefined
  (<*>) = undefined
