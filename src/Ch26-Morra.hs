
module Morra where

import System.Exit
import System.IO
import System.Random

data Command a = Valid a | Quit | Invalid

parseInput :: Char -> Command Int
parseInput ch
  | ch `elem` "Qq" = Quit
  | ch `elem` "12" = Valid $ read [ch]
  | otherwise = Invalid

gameLoop :: IO ()
gameLoop = do
  putStr "P: "
  input <- getChar -- player guess
  _ <- getChar -- really, Haskell? XD
  cpGuess <- randomRIO (1, 2) :: IO Int -- computer guess
  putStrLn ("C: " ++ show cpGuess)
  case parseInput input of
    Quit -> putStrLn "Quitting..." >> exitSuccess
    Invalid -> putStrLn "Type 1, 2 or Q for quit"
    Valid n ->
      case odd $ cpGuess + n of
        True -> putStrLn "- P wins"
        False -> putStrLn "- C wins"
  gameLoop

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "P is player"
  putStrLn "C is computer"
  putStrLn "Player is odds, computer is evens."
  gameLoop
