
module Apl3 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1. Identity

newtype Identity a = Identity a deriving (Show, Eq)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId
genId :: Arbitrary a => Gen (Identity a)
genId = do
  a <- arbitrary
  return $ Identity a

instance Eq a => EqProp (Identity a) where
   (=-=) = eq

-- 2. Pair

data Pair a = Pair a a deriving (Show, Eq)
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')
instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair
genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  a  <- arbitrary
  a' <- arbitrary
  return $ Pair a a'
instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 3. Two
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two m f <*> Two m' x = Two (m <> m') (f x)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo
genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b
instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 4. Three
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' x = Three (a <> a') (b <> b') (f x)
instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
  arbitrary = genThree
genThree :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 5. Three'
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')
instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' m f f' <*> Three' m' x x' = Three' (m <> m') (f x) (f' x')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'
genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  a  <- arbitrary
  b  <- arbitrary
  b' <- arbitrary
  return $ Three' a b b'
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 6. Four
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c)
    => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four ma mb mc f <*> Four ma' mb' mc' x =
    Four (ma <> ma') (mb <> mb') (mc <> mc') (f x)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
  arbitrary = genFour
genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
           => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 7. Four'

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)
instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' ma mb mc f <*> Four' ma' mb' mc' x =
    Four' (ma <> ma') (mb <> mb') (mc <> mc') (f x)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour'
genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four' a b c d
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Test it

type S = String
type I = Int

main :: IO ()
main = do
  putStr "\n-- Identity"
  quickBatch (applicative (undefined :: Identity (I, I, I)))
  putStr "\n-- Pair"
  quickBatch (applicative (undefined :: Pair (I, I, I)))
  putStr "\n-- Two"
  quickBatch (applicative (undefined :: Two (S, S, S) (I, I, I)))
  putStr "\n-- Three"
  quickBatch (applicative (undefined ::
                              Three (S, S, S) (S, S, S) (I, I, I)))
  putStr "\n-- Three'"
  quickBatch (applicative (undefined ::
                              Three' (S, S, S) (I, I, I)))
  putStr "\n-- Four"
  quickBatch (applicative (undefined ::
                              Four (S, S, S)
                                   (S, S, S)
                                   (S, S, S)
                                   (I, I, I)))
  putStr "\n-- Four'"
  quickBatch (applicative (undefined ::
                              Four' (S, S, S)
                                    (I, I, I)))
