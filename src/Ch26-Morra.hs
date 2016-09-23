{-# LANGUAGE OverloadedStrings #-}

module Morra where

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Except
-- import Data.IORef
import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as TL
import System.IO
import System.Random


-- game :: Char -> Text
-- game c = 

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "P is player"
  putStrLn "C is computer"
  putStrLn "Player is odds, computer is evens."
  putStr "P: "
  guess <- getLine
  cpGuess <- randomRIO (1, 2) :: IO Int
  let  cpShow = show cpGuess
  putStrLn ("C: " ++ cpShow)
  case odd $ cpGuess + read guess of
    True -> putStrLn "P wins"
    False -> putStrLn "C wins"

  
  
