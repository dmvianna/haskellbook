{-# LANGUAGE OverloadedStrings #-}

module Morra where

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Except
-- import Data.IORef
-- import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as TL
import System.Exit
import System.IO
import System.Random
-- import Text.Parser.Char
-- import Text.Trifecta

data Command a = Valid a | Quit | Invalid

parseInput :: Char -> Command Int
parseInput ch = do
  case ch of
    'Q' -> Quit
    'q' -> Quit
    '1' -> Valid 1
    '2' -> Valid 2
    _ -> Invalid
    

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "P is player"
  putStrLn "C is computer"
  putStrLn "Player is odds, computer is evens."
  putStr "P: "
  input <- getChar
  cpGuess <- randomRIO (1, 2) :: IO Int -- computer guess
  putStrLn ("C: " ++ show cpGuess)
  case parseInput input of
    Quit -> putStrLn "Quitting..." >> exitSuccess
    Invalid -> putStrLn "Type 1, 2 or Q for quit"
    Valid n ->
      case odd $ cpGuess + n of
        True -> putStrLn "P wins"
        False -> putStrLn "C wins"
  main
