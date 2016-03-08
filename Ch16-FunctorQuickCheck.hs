{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

-- 16.9

functorIdentity :: (Functor f, Eq (f a)) =>
                   f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                   f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1.

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

genId :: Arbitrary a => Gen (Identity a)
genId = do
  x <- arbitrary
  return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

-- 2.

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  x <- arbitrary
  y <- arbitrary
  return $ Pair x y

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

-- 3.

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  x <- arbitrary
  y <- arbitrary
  return $ Two x y

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Two a b) where
        arbitrary = genTwo

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
            Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = genThree

-- 5.


-- main

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  putStrLn "\n [Int]"
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose' :: IntFC)
  putStrLn "\n Identity"
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose' :: IdFC)
  putStrLn "\n Pair"
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose' :: PairFC)
  putStrLn "\n Two"
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  quickCheck (functorCompose' :: TwoFC)
  putStrLn "\n Three"
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  quickCheck (functorCompose' :: ThreeFC)
