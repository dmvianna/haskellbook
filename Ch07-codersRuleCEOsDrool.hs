module TupleFunction where

addEmUp :: Num a => (a, a) -> a
addEmUp (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

funcZ :: (Num a, Eq a) => a -> [Char]
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal :: Eq a => [a] -> [Char]
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

num x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

data Employee = Coder
              | Manager
              | Veep
              | CEO
                deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
