
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk ab a b = ab a == b


arith :: Num b => (a -> b) -> Integer -> a -> b
arith ab i a = (ab a) + (fromInteger i)
