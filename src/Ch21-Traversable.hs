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

-- Big

data Big a b = Big a b b
             deriving (Show, Eq)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'
  foldr f y (Big _ b b') = f b' y

instance Traversable (Big a) where
  traverse f (Big a b b') = (Big a) <$> f b <*> f b'
  sequenceA  (Big a b b') = (Big a) <$>   b <*>   b'
  -- Shorter alternative: sequenceA = traverse id

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- Bigger
data Bigger a b = Bigger a b b b
                deriving (Show, Eq)

instance Functor (Bigger a) where
  fmap f (Bigger a x y z) = Bigger a (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ x y z) = f x <> f y <> f z

instance Traversable (Bigger a) where
  traverse f (Bigger a x y z) = (Bigger a) <$> f x <*> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
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

-- Tree

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
              deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node n x n') = Node (fmap f n) (f x) (fmap f n')

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node n x n') =
    foldMap f n `mappend` f x `mappend` foldMap f n'

  foldr _ y Empty        = y
  foldr f y (Leaf x)     = f x y
  foldr f y (Node l x r) = f x $ foldr f (foldr f y r) l

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node n x n') =
    Node <$> traverse f n <*> f x <*> traverse f n'

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  x <- arbitrary
  n <- genTree
  n' <- genTree
  frequency [ (1, return Empty)
            , (2, return $ Leaf x)
            , (2, return $ Node n x n') ]

--

idTrigger = undefined :: Identity (Int, Int, [Int])
constTrigger = undefined :: Constant Int (Int, Int, [Int])
opTrigger = undefined :: Optional (Int, Int, [Int])
listTrigger = undefined :: List (Int, Int, [Int])
bigTrigger = undefined :: Big Int Int (Int, Int, [Int])
biggerTrigger = undefined :: Bigger Int (Int, Int, [Int])
sTrigger = undefined :: S Maybe (Int, Int, [Int])
treeTrigger = undefined :: Tree (Int, Int, [Int])

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
  putStr "\nBig"
  quickBatch (traversable bigTrigger)
  putStr "\nBigger"
  quickBatch (traversable biggerTrigger)
  putStr "\nS"
  quickBatch (traversable sTrigger)
  putStr "\nTree"
  quickBatch (traversable treeTrigger)
