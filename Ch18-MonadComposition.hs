
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Data.Monoid
import Data.Functor ((<$>))
import Control.Monad (join, (>=>))


mcomp :: (Monad m, Functor m) => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))

mcomp'' :: (Monad m, Functor m) => (b -> m c) -> (a -> m b) -> a -> m c
mcomp'' f g a = g a >>= f

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

-- 18.7 Chapter exercises

-- 1. Nope

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = genNope

genNope :: Gen (Nope a)
genNope = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

-- 2. Yes, I changed the name

data PEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PEither b) where
  fmap f (Main.Left a) = Main.Left (f a)
  fmap f (Main.Right b) = Main.Right b

instance Applicative (PEither b) where
  pure = Main.Left
  Main.Left _ <*> Main.Right x = Main.Right x
  Main.Left f <*> Main.Left x = Main.Left (f x)
  Main.Right x <*> _ = Main.Right x

instance Monad (PEither b) where
  return = pure
  Main.Left a >>= f = f a
  Main.Right b >>= _ = Main.Right b

instance (Arbitrary b, Arbitrary a) => Arbitrary (PEither b a) where
  arbitrary = genPEither

genPEither :: (Arbitrary b, Arbitrary a) => Gen (PEither b a)
genPEither = do
  b <- arbitrary
  a <- arbitrary
  elements [Main.Right b, Main.Left a]

instance (Eq b, Eq a) => EqProp (PEither b a) where (=-=) = eq

-- 3. Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

genId :: Arbitrary a => Gen (Identity a)
genId = do
  x <- arbitrary
  return $ Identity x

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- 4. List

data List a = Nil | Cons a (List a) deriving (Eq, Show)


-- main

type I = Int

main :: IO ()
main = do
  putStr "\n-- Nope"
  quickBatch $ monad (undefined :: Nope (I, I, I))
  putStr "\n-- PEither"
  quickBatch $ monad (undefined :: PEither I (I, I, I))
  putStr "\n-- Identity"
  quickBatch $ monad (undefined :: Identity (I, I, I))
