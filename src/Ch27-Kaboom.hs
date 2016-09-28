
module Kaboom where

import Prelude hiding (foldr)

possiblyKaboom =
  \f -> f fst snd (0, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

foldr k z xs = go xs
  where
    go [] = z
    go (y:ys) = y `k` go ys

c = foldr const 'z' ['a'..'e']
