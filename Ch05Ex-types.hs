
functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i = \a -> a

c :: a -> b -> a
c = \a b -> a

c'' :: b -> a -> b
c'' = \b a -> b

c' :: a -> b -> b
c' = \a b -> b

r :: [a] -> [a]
r = \a -> reverse a

co :: (b -> c) -> (a -> b) -> (a -> c)
co a b c = a (b c)

a :: (a -> c) -> a -> a
a c a = a

a' :: (a -> b) -> a -> b
a' a b = a b
