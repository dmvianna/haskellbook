
import Data.Monoid
import Data.Functor ((<$>))
import Control.Applicative --((<*>))
import qualified Data.Foldable as F

import Data.List (elemIndex)

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2,3), (5,6), (7,8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Short exercises
-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.
-- y :: Maybe Integer
-- y = lookup 3 $ zip [1,2,3] [4,5,6]

-- z :: Maybe Integer
-- z = lookup 2 $ zip [1,2,3] [4,5,6]

-- tupled :: Maybe (Integer, Integer)
-- tupled = (,) <$> y <*> z

-- 3.
-- x :: Maybe Int
-- x = elemIndex 3 [1,2,3,4,5]

-- y :: Maybe Int
-- y = elemIndex 4 [1,2,3,4,5]

-- max' :: Int -> Int -> Int
-- max' = max

-- maxed :: Maybe Int
-- maxed = max' <$> x <*> y

-- 4.
xs = [1,2,3]
ys = [4,5,6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap F.sum $ (,) <$> x <*> y

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

-- Constant

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  a <*> a' = Constant (getConstant a <> getConstant a')

-- Maybe Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a

-- Maybe

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n = if n >= 0
               then Just n
               else Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
    Cow <$> noEmpty name'
        <*> noNegative age'
        <*> noNegative weight'

