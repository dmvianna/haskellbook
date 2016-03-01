
import Data.Monoid (Monoid, mempty, mappend, Sum(..), getSum)
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck (Arbitrary,
                        arbitrary,
                        elements,
                        Gen,
                        quickCheck)

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

-- test ID

testIdEq :: Eq a => Id a -> Id a -> Bool
testIdEq x x' = (\ (Identity a) (Identity a') -> a == a') x x' == (x == x')

-- 3. Two

data Two a b = Two a b deriving Show

-- Two Instances

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) =>
    Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Eq a, Eq b) => Eq (Two a b) where
  Two a b == Two a' b' = (a == a') && (b == b')

instance (Arbitrary a, Monoid a, Semigroup a,
          Arbitrary b, Monoid b, Semigroup b) =>
    Arbitrary (Two a b) where
      arbitrary = genTwo

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

-- test Two

type TwoAssoc = Two S S -> Two S S -> Two S S -> Bool

testTwoEq :: (Eq a, Eq b) => Two a b -> Two a b -> Bool
testTwoEq x x' = (\ (Two a b) (Two a' b') -> (a == a') && (b == b')) x x' ==
                 (x == x')

-- 4. BoolConj

newtype BoolConj = BoolConj Bool

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Eq BoolConj where
  BoolConj a == BoolConj a' = a == a'

instance Show BoolConj where
  show (BoolConj a) = "BoolConj " ++ show a

instance Arbitrary BoolConj where
  arbitrary = genBConj

genBConj :: Gen BoolConj
genBConj = elements [BoolConj False, BoolConj True]

-- test BoolConj

type BCAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

testBCEq :: BoolConj -> BoolConj -> Bool
testBCEq x x' = (\ (BoolConj a) (BoolConj b) -> a == b) x x' == (x == x')

testBCShow :: BoolConj -> Bool
testBCShow x = (\ (BoolConj a) -> "BoolConj " ++ show a) x == show x

testBC :: BoolConj -> BoolConj -> Bool
testBC x x' = (\ (BoolConj a) (BoolConj b) -> a && b) x x' ==
              (\ (BoolConj a) -> a) (x <> x')

-- 5. BoolDisj

newtype BoolDisj = BoolDisj Bool

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Eq BoolDisj where
  BoolDisj a == BoolDisj a' = a == a'

instance Show BoolDisj where
  show (BoolDisj a) = "BoolDisj " ++ show a

instance Arbitrary BoolDisj where
  arbitrary = genBDisj

genBDisj :: Gen BoolDisj
genBDisj = elements [BoolDisj False, BoolDisj True]

-- test BoolDisj

type BDAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

testBDEq :: BoolDisj -> BoolDisj -> Bool
testBDEq x x' = (\ (BoolDisj a) (BoolDisj b) -> a == b) x x' == (x == x')

testBDShow :: BoolDisj -> Bool
testBDShow x = (\ (BoolDisj a) -> "BoolDisj " ++ show a) x == show x

testBD :: BoolDisj -> BoolDisj -> Bool
testBD x x' = (\ (BoolDisj a) (BoolDisj b) -> a || b) x x' ==
              (\ (BoolDisj a) -> a) (x <> x')

-- 6. Or

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> Snd x = Snd x
  Fst _ <> Fst x = Fst x
  Snd x <> Fst _ = Snd x
  Snd x <> Snd _ = Snd x

instance Monoid a => Monoid (Or a b) where
  mempty = Fst mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  elements [Fst a, Snd b]

testOr :: (Eq a, Eq b) => Or a b -> Or a b -> Bool
testOr x x' = case (x, x') of
  (Fst _, Snd a) -> x <> x' == Snd a
  (Fst _, Fst a) -> x <> x' == Fst a
  (Snd a, Fst _) -> x <> x' == Snd a
  (Snd a, Snd _) -> x <> x' == Snd a

type OrAssoc = Or S S -> Or S S -> Or S S -> Bool

-- 7. Combine

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- 8. Comp

newtype Comp a = Comp (a -> a)

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f <> g)

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

-- 9. Mem

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
    }

instance Monoid a => Monoid (Mem s a) where
  mempty = undefined
  mappend = undefined

-- main

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
  quickCheck (testIdEq :: Id S -> Id S -> Bool)
  putStrLn "\n Two"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two S S -> Bool)
  quickCheck (monoidRightIdentity :: Two S S -> Bool)
  quickCheck (testTwoEq :: Two S S -> Two S S -> Bool)
  putStrLn "\n BoolConj"
  quickCheck (semigroupAssoc :: BCAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (testBCEq :: BoolConj -> BoolConj -> Bool)
  quickCheck (testBCShow :: BoolConj -> Bool)
  quickCheck (testBC :: BoolConj -> BoolConj -> Bool)
  putStrLn "\n BoolDisj"
  quickCheck (semigroupAssoc :: BDAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (testBDEq :: BoolDisj -> BoolDisj -> Bool)
  quickCheck (testBDShow :: BoolDisj -> Bool)
  quickCheck (testBD :: BoolDisj -> BoolDisj -> Bool)
  putStrLn "\n Or"
  quickCheck (semigroupAssoc :: OrAssoc)
  putStrLn "No left identity, as Fst a <> Fst b = Fst b"
  quickCheck (monoidLeftIdentity :: Or S S -> Bool)
  quickCheck (monoidRightIdentity :: Or S S -> Bool)
  quickCheck (testOr :: Or S S -> Or S S -> Bool)
