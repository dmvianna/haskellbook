
module Morra where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.IORef
import System.Exit
import System.IO
import System.Random

data Command a = Valid a | Quit | Invalid

type ComputerGuess = Int
type PersonGuess = Int
type ComputerScore = Int
type PersonScore = Int
type Score = (ComputerScore, PersonScore)
type Turn = (ComputerGuess, PersonGuess)
data Game = Game {
  score :: IORef Score
  , turns :: [Turn]
  }

updateScore :: Turn -> Score -> Score
updateScore (cg, pg) =
  if even $ cg + pg
  then first (+1)
  else second (+1)

parseInput :: Char -> Command Int
parseInput ch
  | ch `elem` "Qq" = Quit
  | ch `elem` "12" = Valid $ read [ch]
  | otherwise = Invalid

gameRoutine :: Game -> IO ()
gameRoutine config = do
  putStr "P: "
  input <- getChar -- player guess
  _ <- getChar -- really, Haskell? XD
  cpGuess <- randomRIO (1, 2) :: IO ComputerGuess -- computer guess
  putStrLn ("C: " ++ show cpGuess)
  case parseInput input of
    Quit -> do
      let ref = score config
      score' <- readIORef ref
      putStrLn $ concat [ "Final score -- C: "
                        , show $ fst score'
                        , " P: "
                        , show $ snd score']

      putStrLn "Quitting..."
      exitSuccess
    Invalid -> putStrLn "Type 1, 2 or Q for quit"
    Valid pGuess -> do
      let turn = (cpGuess, pGuess) :: Turn
          ref = score config
          -- turns' = turn : (turns config)
      score' <- readIORef ref
      writeIORef ref (updateScore turn score')
      if odd $ cpGuess + pGuess
      then putStrLn "- P wins"
      else putStrLn "- C wins"

gameScore :: ReaderT Game IO ()
gameScore = do
  config <- ask
  liftIO $ gameRoutine config
  gameScore

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "P is player"
  putStrLn "C is computer"
  putStrLn "Player is odds, computer is evens."
  newScore <- newIORef (0,0)
  let config = Game newScore []
      game r = runReaderT r config
  game gameScore
