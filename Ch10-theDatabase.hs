import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr maybeCons []
    where maybeCons a b =
            case a of
              (DbDate date) -> date : b
              _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber x:[]) = [x]
filterDbNumber (_:[]) = []
filterDbNumber (DbNumber x:xs) = x:filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral
          (foldr (\a b -> a + b) 0 $ filterDbNumber x)
          / (fromIntegral $ length $ filterDbNumber x)

avgDb2 :: [DatabaseItem] -> Double
avgDb2 x = (fromIntegral $ sumDb x) /
           (fromIntegral $ length $ filterDbNumber x)

fDbDate :: [DatabaseItem] -> [UTCTime]
fDbDate x =
  case x of
    [] -> []
    DbDate x:[] -> [x]
    _:[] -> []
    DbDate x:xs -> x:fDbDate xs
    _:xs -> fDbDate xs
    
fibs = 1 : scanl (+) 1 fibs
fibsN x = take x fibs

fibsLess :: Int -> [Int]
fibsLess x = go x 0 []
    where go n ix xs
              | n > fibs !! ix = fibs !! ix : go x (ix + 1) xs
              | n <= fibs !! ix = xs

----------------

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = any ((==) x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- myMaximumBy f = foldr (\a b -> if f a b == GT then a else b)
