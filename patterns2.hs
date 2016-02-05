tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10


potDig :: Integral a => a -> a -> a
potDig x y = d
    where (xLast, _) = divMod x $ 10 ^ (y - 1)
          (_, d) = divMod xLast $ 10

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
