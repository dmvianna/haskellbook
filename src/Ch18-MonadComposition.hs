
import           Control.Applicative
import           Control.Monad            (join, (>=>))
import           Data.Functor
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


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
  fmap f (Main.Left a)  = Main.Left (f a)
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

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil lb         = lb
    mappend (Cons a la) lb = Cons a (la `mappend` lb)

instance Applicative List where
    pure a = Cons a Nil

    (<*>) Nil _          = Nil
    (<*>) (Cons f fs) as = mappend (fmap f as) (fs <*> as)

instance Monad List where
  return x = Cons x Nil
  Nil >>= _f = Nil
  Cons head tail >>= f = append (f head) (tail >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [(3, return $ Cons h t),
             (1, return Nil)]

instance Eq a => EqProp (List a) where (=-=) = eq

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
  putStr "\n-- List"
  quickBatch $ monad (undefined :: List (I, I, I))

-- Functions

j :: Monad m => m (m a) -> m a
j = (=<<) id

l1 :: (Functor m, Monad m) => (a -> b) -> m a -> m b
l1 = fmap

l2 :: (Applicative m, Monad m) => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: (Applicative m, Monad m) => m a -> m (a -> b) -> m b
a = flip (<*>)

-- meh ≡ flip (mapM f as)
meh :: (Functor m, Monad m) => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  x' <- f x
  fmap ((:) x') (meh xs f)

flipType :: (Functor m, Monad m) => [m a] -> m [a]
flipType = (flip meh) id
