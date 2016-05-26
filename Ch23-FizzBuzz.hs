
import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Fizz"
           | n `mod` 3  == 0 = "Buzz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []


addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo x y
    | x == y = fizzBuzzList [x]
    | x < y && y - x == 1 = fizzBuzzList [y,x]
    | x < y = fizzBuzzList [y, y - 1 .. x]
    | otherwise = fizzbuzzFromTo y x

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100
