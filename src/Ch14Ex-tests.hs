
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

-- Exercises: using QuickCheck

-- 1. Division

divisor :: Gen Float
divisor = arbitrary `suchThat` (/= 0)

half x = x / 2
halfIdentity = (*2) . half

prop_half :: Property
prop_half =
  forAll divisor
  (\x -> (half x) * 2 == x)

prop_identity :: Property
prop_identity =
  forAll divisor
  (\x -> (halfIdentity x) == x)

-- 2. Sorting

genList :: (Arbitrary a, Eq a) => Gen [a]
genList = do
  a <- arbitrary
  b <- arbitrary `suchThat` (/= a)
  c <- arbitrary `suchThat` (`notElem` [a, b])
  return [a, b, c]

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: Property
prop_listOrdered =
    forAll (genList :: Gen String)
    (\x -> listOrdered $ sort x)

-- 3. Addition

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = x `f` (y `f` z) == (x `f` y) `f` z

commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutative f x y = x `f` y == y `f` x

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = arbitrary

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
               Gen (a, b, c)
genThreeple = arbitrary

untrurry :: (a -> b -> c -> d) -> ((a, b, c) -> d)
untrurry f (a, b, c) = f a b c

prop_plusAssoc :: Property
prop_plusAssoc =
  forAll (genThreeple :: Gen (Int, Int, Int))
  (untrurry $ associative (+))

prop_plusComm :: Property
prop_plusComm =
  forAll (genTuple :: Gen (Int, Int))
  (uncurry $ commutative (+))

-- 4. Multiplication

prop_timesAssoc :: Property
prop_timesAssoc =
  forAll (genThreeple :: Gen (Int, Int, Int))
  (untrurry $ associative (*))

prop_timesComm :: Property
prop_timesComm =
  forAll (genTuple :: Gen (Int, Int))
  (uncurry $ commutative (*))

-- 5. div vs mod

quotVsRem :: Integral a => a -> a -> Bool
quotVsRem x y = (quot x y) * y + (rem x y) == x

divVsMod :: Integral a => a -> a -> Bool
divVsMod x y = (div x y) * y + (mod x y) == x

genTupleNonZero :: (Arbitrary a, Num a, Eq a) => Gen (a, a)
genTupleNonZero = do
  x <- arbitrary `suchThat` (/= 0)
  y <- arbitrary `suchThat` (/= 0)
  return (x, y)

prop_quotRem :: Property
prop_quotRem =
  forAll (genTupleNonZero :: Gen (Int, Int))
  (uncurry quotVsRem)

prop_divMod :: Property
prop_divMod =
  forAll (genTupleNonZero :: Gen (Int, Int))
  (uncurry divVsMod)

-- 6. (^)

genTuplePos :: (Arbitrary a, Num a, Ord a) => Gen (a, a)
genTuplePos = do
  x <- arbitrary `suchThat` (> 1)
  y <- arbitrary `suchThat` (> 1)
  return (x, y)

genThreeplePos :: (Arbitrary a, Num a, Ord a) => Gen (a, a, a)
genThreeplePos = do
  x <- arbitrary `suchThat` (> 1)
  y <- arbitrary `suchThat` (> 1)
  z <- arbitrary `suchThat` (> 1)
  return (x, y, z)

prop_hatAssoc :: Property
prop_hatAssoc =
  forAll (genThreeplePos :: Gen (Int, Int, Int))
  (untrurry $ associative (^))

prop_hatComm :: Property
prop_hatComm =
  forAll (genTuplePos :: Gen (Int, Int))
  (uncurry $ commutative (^))

-- 7. reverse reverse list == list

prop_reverse :: Property
prop_reverse =
  forAll (genList :: Gen [Int])
  (\x -> (reverse . reverse) x == id x)

-- 8. ($)

prop_dollar :: Property
prop_dollar =
  forAll divisor
  (\x -> ((-) x $ x + x) == (-) x (x + x))

-- 9. Check functions

prop_concat :: Property
prop_concat =
  forAll (genTuple :: Gen ([Int], [Int]))
  (\(x, y) -> foldr (:) y x == (++) x y)

prop_concat' :: Property
prop_concat' =
  forAll (genTuple :: Gen ([Int], [Int]))
  (\(x, y) -> foldr (++) [] [x, y] == concat [x, y])

-- 10. Check property

prop_lengthTake :: Property
prop_lengthTake =
  forAll (genTuple :: Gen (Int, [Int]))
  (\(n, xs) -> length (take n xs) == n)

-- 11. show . read

prop_showRead :: Property
prop_showRead =
  forAll (genList :: Gen String)
  (\x -> (read (show x)) == x)

-- Failure

genPos :: (Num a, Arbitrary a, Ord a) => Gen a
genPos = arbitrary `suchThat` (> 0)

square x = x * x
squareId = square . sqrt

prop_square :: Property
prop_square =
  forAll (genPos :: Gen Float)
  (\x -> squareId x == x)

-- Idempotence

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = map toUpper

prop_capitalizeWord :: Property
prop_capitalizeWord =
  forAll (genList :: Gen String)
  (\x -> capitalizeWord x == twice capitalizeWord x
         &&
         capitalizeWord x == fourTimes capitalizeWord x)

prop_sort :: Property
prop_sort =
  forAll (genList :: Gen String)
  (\x -> sort x == twice sort x
         &&
         sort x == fourTimes sort x)

-- Make Gen for Fool

data Fool = Fulse | Frue deriving (Eq, Show)

-- 1. Equal probabilities

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

-- 2. 2/3 Fulse, 1/3 Frue

genUnfair :: Gen Fool
genUnfair = elements [Frue, Fulse, Fulse]

-- Main

main :: IO ()
main = do
  putStrLn "\nhalf"
  quickCheck prop_half
  putStrLn "\nhalfIdentity"
  quickCheck prop_identity
  putStrLn "\nCheck ordering"
  quickCheck prop_listOrdered
  putStrLn "\nCheck plusAssociative"
  quickCheck prop_plusAssoc
  putStrLn "\nCheck plusCommutative"
  quickCheck prop_plusComm
  putStrLn "\nCheck timesAssociative"
  quickCheck prop_timesAssoc
  putStrLn "\nCheck timesCommutative"
  quickCheck prop_timesComm
  putStrLn "\nCheck quotVsRem"
  quickCheck prop_quotRem
  putStrLn "\nCheck divVsMod"
  quickCheck prop_divMod
  putStrLn "\nCheck if exponentiation is commutative"
  quickCheck prop_hatComm
  putStrLn "\nCheck if exponentiation is associative"
  quickCheck prop_hatAssoc
  putStrLn "\nCheck if reverse . reverse == id"
  quickCheck prop_reverse
  putStrLn "\nCheck ($)"
  quickCheck prop_dollar
  putStrLn "\nCompare foldr (:) and (++)"
  quickCheck prop_concat
  putStrLn "\nCompare foldr (++) [] and concat"
  quickCheck prop_concat'
  putStrLn "\nCheck length n take == n"
  quickCheck prop_lengthTake
  putStrLn "\nCheck show . read == id"
  quickCheck prop_showRead
  putStrLn "\nCheck square . sqrt with Float"
  quickCheck prop_square
  putStrLn "\nCheck idempotence capitalizeWord"
  quickCheck prop_capitalizeWord
  putStrLn "\nCheck idempotence sort"
  quickCheck prop_sort
