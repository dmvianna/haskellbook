
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

-- 4.

data Three a b c = Three a b c

-- instances

instance (Semigroup a, Semigroup b, Semigroup c)
    => Semigroup (Three a b c) where
        Three a b c <> Three a' b' c' =
            Three (a <> a') (b <> b') (c <> c')

instance (Eq a, Eq b, Eq c) => Eq (Three a b c) where
    Three a b c == Three a' b' c' =
        (a == a') && (b == b') && (c == c')

instance (Show a, Show b, Show c) => Show (Three a b c) where
  show (Three a b c) =
    "Three " ++ show a ++ " " ++ show b ++ " " ++ show c

-- tests

testThreeEq :: (Eq a, Eq b, Eq c) => Three a b c -> Three a b c -> Bool
testThreeEq x x' = (x == x') == (\(Three a b c) (Three a' b' c') ->
                                (a == a') && (b == b') && (c == c')) x x'

testThreeShow :: (Show a, Show b, Show c) => Three a b c -> Bool
testThreeShow x =
    show x ==
           (\(Three a b c) ->
                "Three " ++ show a ++ " " ++ show b ++ " " ++ show c) x

-- generators

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c)
          => Gen (Three a b c)
genThree = do
 x <- arbitrary
 y <- arbitrary
 z <- arbitrary
 return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
  arbitrary = genThree

-- 5.

data Four a b c d = Four a b c d

-- instances

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
        Four a b c d <> Four a' b' c' d' =
            Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Eq a, Eq b, Eq c, Eq d) => Eq (Four a b c d) where
    Four a b c d == Four a' b' c' d' =
        (a == a') && (b == b') && (c == c') && (d == d')

instance (Show a, Show b, Show c, Show d) => Show (Four a b c d) where
  show (Four a b c d) =
    "Four " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d

-- tests

testFourEq :: (Eq a, Eq b, Eq c, Eq d) => Four a b c d -> Four a b c d -> Bool
testFourEq x x' = (x == x') ==
                  (\(Four a b c d) (Four a' b' c' d') ->
                   (a == a') && (b == b') && (c == c') && (d == d')) x x'

testFourShow :: (Show a, Show b, Show c, Show d) => Four a b c d -> Bool
testFourShow x =
    show x ==
           (\(Four a b c d) ->
                "Four " ++ show a ++ " " ++ show b ++
                " " ++ show c ++ " " ++ show d) x

-- generators

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
          => Gen (Four a b c d)
genFour = do
  w <- arbitrary
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Four w x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
  arbitrary = genFour


-- main

type S = String
type Id = Identity
type IdentityAssoc = Id S -> Id S -> Id S -> Bool
type IdentityEq = Id S -> Id S -> Bool
type TwoAssoc = Two S S -> Two S S -> Two S S -> Bool
type TwoEq = Two S S -> Two S S -> Bool
type ThreeAssoc = Three S S S -> Three S S S -> Three S S S -> Bool
type ThreeEq = Three S S S -> Three S S S -> Bool
type FourAssoc = Four S S S S -> Four S S S S -> Four S S S S -> Bool
type FourEq = Four S S S S -> Four S S S S -> Bool


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
  putStrLn "\n Three"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (testThreeEq :: ThreeEq)
  quickCheck (testThreeShow :: Three S S S -> Bool)
  putStrLn "\n Four"
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (testFourEq :: FourEq)
  quickCheck (testFourShow :: Four S S S S -> Bool)

