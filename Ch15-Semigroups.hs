
import Data.Semigroup
import Test.QuickCheck

-- Semigroup exercises
-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity (a <> a')

instance Eq a => Eq (Identity a) where
  Identity a == Identity a' = a == a'

testIdEq :: Eq a => Identity a -> Identity a -> Bool
testIdEq x x' = (x == x') == (\(Identity a) (Identity b) -> a == b) x x'


instance (Show a) => Show (Identity a) where
  show (Identity a) = show a

testIdShow :: Show a => Identity a -> Bool
testIdShow x = show x == (\(Identity a) -> show a) x

genId :: Arbitrary a => Gen (Identity a)
genId = do
 x <- arbitrary
 return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

-- 3.

data Two a b = Two a b

-- instances

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Eq a, Eq b) => Eq (Two a b) where
  Two a b == Two a' b' = (a == a') && (b == b')

instance (Show a, Show b) => Show (Two a b) where
  show (Two a b) = "Two " ++ show a ++ " " ++ show b 

-- tests

testTwoEq :: (Eq a, Eq b) => Two a b -> Two a b -> Bool
testTwoEq x x' = (x == x') == (\(Two a b) (Two a' b') ->
                                (a == a') && (b == b')) x x'

testTwoShow :: (Show a, Show b) => Two a b -> Bool
testTwoShow x = show x ==
                (\(Two a b) -> "Two " ++ show a ++ " " ++ show b) x

-- generators

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
 x <- arbitrary
 y <- arbitrary
 return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

-- main

type S = String
type Id = Identity
type IdentityAssoc = Id S -> Id S -> Id S -> Bool
type IdentityEq = Id S -> Id S -> Bool
type TwoAssoc = Two S S -> Two S S -> Two S S -> Bool
type TwoEq = Two S S -> Two S S -> Bool

main :: IO ()
main = do
  putStrLn "\nTrivial"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  putStrLn "\nIdentity"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (testIdEq :: IdentityEq)
  quickCheck (testIdShow :: Id String -> Bool)
  putStrLn "\nTwo"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (testTwoEq :: TwoEq)
  quickCheck (testTwoShow :: Two Int Float -> Bool)
