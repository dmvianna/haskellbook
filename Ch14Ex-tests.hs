
import Test.QuickCheck
import Data.List (sort)

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

genList :: Gen [Int]
genList = do
  a <- arbitrary
  return [a, a, a, a, a]

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: Property
prop_listOrdered =
    forAll genList
    (\x -> listOrdered $ sort x)

-- 3. Addition

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

genTuple :: Arbitrary a => Gen (a, a)
genTuple = arbitrary

genThreeple :: Arbitrary a => Gen (a, a, a)
genThreeple = arbitrary

untrurry :: (a -> b -> c -> d) -> ((a, b, c) -> d)
untrurry f (a, b, c) = f a b c

prop_plusAssoc :: Property
prop_plusAssoc =
  forAll (genThreeple :: Gen (Int, Int, Int))
  (untrurry plusAssociative)

prop_plusComm :: Property
prop_plusComm =
  forAll (genTuple :: Gen (Int, Int))
  (uncurry plusCommutative)

-- 4. Multiplication

timesAssociative x y z = x * (y * z) == (x * y) * z
timesCommutative x y = x * y == y * x

prop_timesAssoc :: Property
prop_timesAssoc =
  forAll (genThreeple :: Gen (Int, Int, Int))
  (untrurry timesAssociative)

prop_timesComm :: Property
prop_timesComm =
  forAll (genTuple :: Gen (Int, Int))
  (uncurry timesCommutative)

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

-- common stuff

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
