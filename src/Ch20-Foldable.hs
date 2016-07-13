
import Data.Foldable
import Data.Monoid

data Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

mySum :: (Foldable t, Num a) => t a -> a
mySum x = getSum $ foldMap Sum x

myProduct :: (Foldable t, Num a) => t a -> a
myProduct x = getProduct $ foldMap Product x

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem x xs = getAny $ foldMap (Any . (== x)) xs

data Min a = Min { getMin :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min Nothing `mappend` x = x
  x `mappend` Min Nothing = x
  mappend (Min a) (Min a') = Min (min a a')

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum x = getMin $ foldMap (Min . Just) x

data Max a = Max { getMax :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max Nothing `mappend` x = x
  x `mappend` Max Nothing = x
  mappend (Max a) (Max a') = Max (max a a')

myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
myMaximum x = getMax $ foldMap (Max . Just) x

myNull :: (Foldable t) => t a -> Bool
myNull = foldr (\_ _ -> False) True

myLength :: (Foldable t) => t a -> Int
myLength = foldr (\_ b -> b + 1) 0

myToList :: (Foldable t) => t a -> [a]
myToList = foldr (\a b -> a : b) []

myFold :: (Foldable t, Monoid m) => t m -> m
myFold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr (mappend . f) mempty

-- 20.6 Chapter Exercises

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = mappend (f b) (f b')

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a x y z) = f x `mappend` f y `mappend` f z

filterF :: (Applicative f, Foldable f, Monoid (f a)) =>
           (a -> Bool) -> f a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)
