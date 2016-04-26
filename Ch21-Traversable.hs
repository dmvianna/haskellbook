
import Prelude hiding (Either, Left, Right)
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

--

idTrigger = undefined :: Identity (Int, Int, [Int])
constTrigger = undefined :: Constant Int (Int, Int, [Int])

main :: IO ()
main = do
  putStr "\nIdentity"
  quickBatch (traversable idTrigger)
  putStr "\nConstant"
  quickBatch (traversable constTrigger)
