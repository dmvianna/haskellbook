
import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck (quickCheck, Arbitrary, arbitrary, Gen)

-- Monoid exercises

-- Semigroup and Monoid tests

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

-- 1. Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2. Identity

newtype Identity a = Identity a deriving Show

-- Id Instances

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity (a <> a')

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Eq a => Eq (Identity a) where
  Identity a == Identity a' = a == a'

instance (Arbitrary a, Monoid a, Semigroup a) =>
    Arbitrary (Identity a) where
      arbitrary = genId

-- ID generator

genId :: Arbitrary a => Gen (Identity a)
genId = do
  x <- arbitrary
  return $ Identity x

type S = String
type Id = Identity
type IdAssoc = Id S -> Id S -> Id S -> Bool 

main :: IO ()
main = do
  putStrLn "\n Trivial"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "\n Identity"
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (monoidLeftIdentity :: Id S -> Bool)
  quickCheck (monoidRightIdentity :: Id S -> Bool)
