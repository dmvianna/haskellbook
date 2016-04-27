{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (Either, Left, Right)
import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Either

data Either a b = Left a
                | Right b
                  deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right y) = f y

  foldr _ z (Left _) = z
  foldr f z (Right y) = f y z

instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y

-- Tuple

-- instance Functor ((,) a) where
--   fmap f (x, y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) = (u `mappend` v, f x)

-- instance Traversable ((,) a) where
--   traverse f (x, y) = (,) x <$> f y

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

genId :: Arbitrary a => Gen (Identity a)
genId = do
  x <- arbitrary
  return $ Identity x

-- Constant

newtype Constant a b = Constant { getConstant :: a }
    deriving (Show, Eq)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  x <*> x' = Constant (getConstant x `mappend` getConstant x')

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x

instance (Eq a, Eq b) => EqProp (Constant a b) where
  x =-= x' = getConstant x `eq` getConstant x'

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = genConst

genConst :: Arbitrary a => Gen (Constant a b)
genConst = do
  a <- arbitrary
  return $ Constant a

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  Nada `mappend` _ = Nada
  _ `mappend` Nada = Nada
  Yep x `mappend` Yep x' = Yep (x `mappend` x')

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
  pure = Yep
  Nada <*> _ = Nada
  _ <*> Nada = Nada
  (Yep f) <*> (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap f Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [ (1, return Nada)
                        , (2, genYep) ]

genYep :: Arbitrary a => Gen (Optional a)
genYep = do
  x <- arbitrary
  return $ Yep x

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons h t) = f h `mappend` foldMap f t

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons h t) = Cons <$> f h <*> traverse f t

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [ (3, return $ Cons h t)
            , (1, return Nil) ]

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = genThree

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
            Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

-- Three'

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b `mappend` f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  a <- arbitrary
  b <- arbitrary
  b' <- arbitrary
  return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- S

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a
  
instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

instance (Arbitrary (n a), CoArbitrary (n a),
          Arbitrary a, CoArbitrary a) =>
    Arbitrary (S n a) where
  arbitrary = genS

genS :: (Arbitrary (n a), CoArbitrary (n a),
         Arbitrary a, CoArbitrary a) =>
        Gen (S n a)
genS = do
  n <- arbitrary
  a <- arbitrary
  return $ S (n a) a

--

idTrigger = undefined :: Identity (Int, Int, [Int])
constTrigger = undefined :: Constant Int (Int, Int, [Int])
opTrigger = undefined :: Optional (Int, Int, [Int])
listTrigger = undefined :: List (Int, Int, [Int])
threeTrigger = undefined :: Three Int Int (Int, Int, [Int])
three'Trigger = undefined :: Three' Int (Int, Int, [Int])
sTrigger = undefined :: S Maybe (Int, Int, [Int])

main :: IO ()
main = do
  putStr "\nIdentity"
  quickBatch (traversable idTrigger)
  putStr "\nConstant"
  quickBatch (traversable constTrigger)
  putStr "\nOptional"
  quickBatch (traversable opTrigger)
  putStr "\nList"
  quickBatch (traversable listTrigger)
  putStr "\nThree"
  quickBatch (traversable threeTrigger)
  putStr "\nThree'"
  quickBatch (traversable three'Trigger)
  putStr "\nS"
  quickBatch (traversable sTrigger)
