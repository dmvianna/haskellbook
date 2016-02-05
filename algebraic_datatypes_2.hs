{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Data.Int

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany, Num)

instance TooMany (Int, String) where
  tooMany (n,_) = n > 42

-- instance TooMany (Int, Int) where
--   tooMany (x, y) = (x + y) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)



data NumberOrBool =
    Numba Int8
        | BoolyBool Bool
          deriving (Eq, Show)

-- data Person = MkPerson String Int deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

-- namae :: Person -> String
-- namae (MkPerson s _) = s

data Person =
  Person { name :: String
         , age :: Int }
  deriving (Eq, Show)

