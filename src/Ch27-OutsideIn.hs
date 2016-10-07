
module OutsideIn where

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"

hypo' :: IO ()
hypo' = do
  let x :: Integer
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _ -> putStrLn "hello"

data Test = A Test2
          | B Test2
            deriving (Show)

data Test2 = C Int
           | D Int
             deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (B (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (D i)) = i

discriminatory :: Bool -> Int
discriminatory b =
  let x = undefined
  in case b of
    False -> 0
    True -> 1

hypo'' :: IO ()
hypo'' = do
  let x :: Integer
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"


