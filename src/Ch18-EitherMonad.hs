
module EitherMonad where

import Control.Applicative

-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop =
  Shop {
      founded     :: Founded
    , programmers :: Coders
    } deriving (Eq, Show)

data FoundedError = NegativeYears Founded
                  | TooManyYears Founded
                  | NegativeCoders Coders
                  | TooManyCoders Coders
                  | TooManyCodersForYears Founded Coders
                    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

-- Tho, many programmers *are* negative.
validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
  then Left $ TooManyCodersForYears founded programmers
  else Right $ Shop founded programmers

-- Either monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First  x <*> _        = First x
  Second _ <*> First  x = First x
  Second f <*> Second x = Second (f x)

instance Monad (Sum a) where
  return = pure
  First  x >>= _  = First x
  Second x >>= f  = f x

