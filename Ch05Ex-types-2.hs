f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h a = g (f a)

  
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz a, yz b)

-- Mash Until No Good

data W

ywz :: Y -> (W, Z)
ywz = undefined

xy :: X -> Y
xy = undefined

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywz x = fst (ywz (xy x))


divideThenAdd :: Fractional a => a -> a -> a 
divideThenAdd x y = (x / y) + 1
