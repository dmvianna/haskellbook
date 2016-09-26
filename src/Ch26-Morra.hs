
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
data Mode = P2P | AI2P
data Game = Game {
  score :: IORef Score
  , mode :: Mode
  , turns :: [Turn]
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

gameRoutine :: Game -> IO ()
gameRoutine config = do
  let ref = score config
  score' <- readIORef ref
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
          -- turns' = turn : (turns config)
      writeIORef ref (updateScore turn score')
      putStrLn $ turnWinner turn

app :: ReaderT Game IO ()
app = do
  config <- ask
  liftIO $ gameRoutine config
  app

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "P is player"
  putStrLn "C is computer"
  putStrLn "Player is odds, computer is evens."
  newScore <- newIORef (0,0)
  let config = Game newScore AI2P []
      run r = runReaderT r config
  run app
