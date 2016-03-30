
module Apl2 where

import Control.Applicative
import Data.Monoid hiding (Sum, First)
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)
data Validation e a = Error e | Success a deriving (Eq, Show)

-- Sum instances

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Applicative (Sum a) where
  pure = Second
  First a <*> First _ = First a
  First a <*> Second _ = First a
  Second _ <*> First a = First a
  Second f <*> Second b = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = genSum

genSum :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genSum = do
  a <- arbitrary
  b <- arbitrary
  elements [First a, Second b]

-- Validation instances

instance Functor (Validation e) where
  fmap f (Success a) = Success (f a)
  fmap _ (Error e) = Error e

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Error e <*> Error e' = Error (e <> e')
  Error e <*> Success _ = Error e
  Success _ <*> Error e = Error e
  Success f <*> Success a = Success (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = genValidation

genValidation :: (Arbitrary e, Arbitrary a) => Gen (Validation e a)
genValidation = do
  e <- arbitrary
  a <- arbitrary
  elements [Error e, Success a]

-- Test it

instance (Eq e, Eq a) => EqProp (Validation e a) where
    Error e =-= Error e' = e `eq` e'
    Success a =-= Success a' = a `eq` a'
    x =-= y = x `eq` y


-- main :: IO ()
-- main = do
--   putStrLn "-- applicative Validation"
--   quickBatch (applicative $ undefined :: (Sum Int Int,
--                                           Validation String Int,
--                                           Sum Int String))



-- applyIfBothSecond :: (Sum e) (a -> b)
--                   -> (Sum e) a
--                   -> (Sum e) b

-- applyMappendError :: Monoid e =>
--                      (Validation e) (a -> b)
--                   -> (Validation e) a
--                   -> (Validation e) b

