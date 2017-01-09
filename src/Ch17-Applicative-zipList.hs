
module Apl1 where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

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
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend = append

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let l = xs
                    in take' 3000 l
              ys' = let l = ys
                    in take' 3000 l

-- ZipList Applicative

take' :: Int -> List a -> List a
take' n xs = f n xs Nil
    where f n' (Cons h t) acc =
              if n' == 0
              then acc
              else f (n' - 1) t (Cons h acc)
          f _ Nil acc = acc

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
  pure x = ZipList' (Cons x Nil)
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' Nil <*> _ = ZipList' Nil
  ZipList' (Cons f Nil) <*> ZipList' (Cons x xs) =
    ZipList' $ Cons (f x) (pure f <*> xs)
  ZipList' (Cons f fs) <*> ZipList' (Cons x Nil) =
    ZipList' $ Cons (f x) (fs <*> pure x)
  ZipList' (Cons f fs) <*> ZipList' (Cons x xs) =
    ZipList' $ Cons (f x) (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = genZipList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [(3, return $ Cons h t),
             (1, return Nil)]

genZipList :: Arbitrary a => Gen (ZipList' a)
genZipList = do
  l <- arbitrary
  return $ ZipList' l

main :: IO ()
main = do
  putStrLn "-- applicative ZipList'"
  quickBatch (applicative $ ZipList' (Cons (undefined :: (Bool, Bool, Bool)) Nil))
