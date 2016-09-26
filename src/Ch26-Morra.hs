
module Morra where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.IORef
import System.Exit
import System.IO
import System.Random

data Command a = Valid a | Quit | Invalid

type AIGuess = Int
type PersonGuess = Int
type AIScore = Int
type PersonScore = Int
type Score = (AIScore, PersonScore)
type Turn = (AIGuess, PersonGuess)
data Mode = AI2P | P2P
data GameState = GameState { score :: Score
                 , turns :: [Turn]
                 }
data Game = Game {
  gameState :: IORef GameState
  , mode :: Mode
  }

updateScore :: Turn -> Score -> Score
updateScore (cg, pg) =
  if even $ cg + pg
  then first (+1)
  else second (+1)

gameWinner :: Score -> String
gameWinner s =
  case uncurry compare s of
    GT -> "Beaten by the AI!"
    EQ -> "It is a draw!"
    LT -> "Way to go, human!"

turnWinner :: Turn -> String
turnWinner (c, p) =
  if odd $ c + p
  then "- P wins"
  else "- C wins"

parseInput :: Char -> Command Int
parseInput ch
  | ch `elem` "Qq" = Quit
  | ch `elem` "12" = Valid $ read [ch]
  | otherwise = Invalid

parseMode :: Char -> Either String Mode
parseMode ch
  | ch `elem` "Pp" = Right P2P
  | ch `elem` "Cc" = Right AI2P
  | otherwise = Left $ "Key pressed: " ++ [ch]

gameRoutine :: Game -> IO ()
gameRoutine (Game ref AI2P) = do
  st <- readIORef ref
  let score' = score st
      turns' = turns st

  putStr "P: " -- prompt person to play
  input <- getChar -- person guess
  _ <- getChar -- consuming newline, so it doesn't come back later
  aiGuess <- randomRIO (1, 2) :: IO AIGuess -- AI guess
  putStrLn ("C: " ++ show aiGuess) -- reveal AI guess
  case parseInput input of
    Invalid -> putStrLn "Type 1, 2 or Q for quit"
    Quit -> do
      putStrLn $ concat [ "Final score -- C: "
                        , show $ fst score'
                        , " P: "
                        , show $ snd score']
      putStrLn $ gameWinner score'
      putStrLn "Quitting..."
      exitSuccess
    Valid pGuess -> do
      let turn = (aiGuess, pGuess) :: Turn
      writeIORef ref $ GameState (updateScore turn score') (turn:turns')
      putStrLn $ turnWinner turn

app :: ReaderT Game IO ()
app = do
  config <- ask
  liftIO $ gameRoutine config
  app

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "*********Set game mode: *********"
  putStrLn "* P for Person to Person        *"
  putStrLn "* C for Person vs AI (Computer) *"
  putStrLn "******any other key to quit******"
  m <- getChar
  _ <- getChar
  case parseMode m of
    Left e -> putStrLn e >> exitSuccess
    Right m' -> do
      newGame <- newIORef $ GameState (0,0) []
      let config = Game newGame m'
          run r = runReaderT r config
      run app
