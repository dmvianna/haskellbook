
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

-- Typechecks, but doesn't sort yet
myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum x = getFirst $ foldMap (First . Just) x
