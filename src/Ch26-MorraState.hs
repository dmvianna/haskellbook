
module MorraState where

-- import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
-- import Control.Monad.Trans.State.Lazy
import Data.Word8
-- import Data.Bifunctor
-- import Data.IORef
import System.Exit
import System.IO
import System.Random

data Player = A | B deriving Show

data Guess = Odd | Even deriving (Bounded, Enum, Eq, Show)

data Turn = Turn { turnA :: Guess
                 , turnB :: Guess }
          deriving Show

data Score = Score { scoreA :: Word8
                   , scoreB :: Word8 }
           deriving Show

data Mode = AI2P | P2P

-- Scores

updateScore :: Player -> Score -> Score
updateScore p s =
  case p of
    A -> Score { scoreA = scoreA s + 1, scoreB = scoreB s }
    B -> Score { scoreA = scoreA s, scoreB = scoreB s + 1 }

getWinner :: Turn -> Player
getWinner (Turn a b) =
  if a == b
  then A
  else B

getScore :: [Turn] -> Score
getScore = foldr (updateScore . getWinner) (Score 0 0)

-- Random generator

instance Random Guess where
  random g = case randomR (fromEnum Odd, fromEnum Even) g of
    (r, g') -> (toEnum r, g')
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')

-- AI

aiTurn :: [Turn] -> IO Guess
aiTurn ts =
  if length ts < 3
  then guess
  else
    let pattern = take 2 $ turnB <$> ts
        recall = lookup pattern $ trigrams ts
    in case recall of
      Nothing -> guess
      Just r -> return r
  where guess = randomIO :: IO Guess

trigrams :: [Turn] -> [([Guess], Guess)]
trigrams ts =
  if length ts < 3
  then []
  else
    let [c,b,a] = take 3 $ turnB <$> ts
    in ([b,a],c) : trigrams (tail ts)

-- As we approach IO land, things get messier

type Name = String

player :: Mode -> Player -> Name
player AI2P A = "Computer"
player AI2P B = "Person"
player P2P  A = "Person 1"
player P2P  B = "Person 2"

printRules :: ReaderT Mode IO ()
printRules = do
  m <- ask
  liftIO $ do
    putStrLn "Press 1 for Odds, 2 for evens."
    putStrLn $ player m A ++ " is evens,"
    putStrLn $ player m B ++ " is odds."

promptInput :: Name -> IO Char
promptInput n = do
  putStr $ n ++ ": "
  c <- getChar
  _ <- getChar
  return c

parseInput :: Char -> ExceptT (IO ()) IO Guess
parseInput c
  | c `elem` "Qq" = throwE exitSuccess
  | c `elem` "12" = return $ (toEnum . (subtract 1) . read) [c]
  | otherwise = throwE $ putStrLn "Press 1 for Odd, 2 for Even, and Q for Quit"

parseMode :: Char -> ExceptT (IO ()) IO Mode
parseMode c
  | c `elem` "Pp" = return $ P2P
  | c `elem` "Cc" = return $ AI2P
  | otherwise = throwE $
                (putStrLn $ "Key pressed: " ++ [c])
                >> putStrLn "Quitting..."
                >> exitSuccess

promptMode :: IO Char
promptMode = do
  putStrLn "*********Set game mode: *********"
  putStrLn "* P for Person to Person        *"
  putStrLn "* C for Person vs AI (Computer) *"
  putStrLn "******any other key to quit******"
  putStr "Selection: "
  c <- getChar
  _ <- getChar
  return c

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  m'' <- promptMode
  m' <- runExceptT $ parseMode m''
  case m' of
    Right m -> do -- move to separate definitions
      runReaderT printRules m
      runReaderT personGuess m
    Left e -> e

personGuess :: ReaderT Mode IO ()
personGuess = do
  m <- ask
  c <- liftIO $ promptInput $ player m A
  c' <- liftIO $ runExceptT $ parseInput c
  case c' of
    Right x -> liftIO $ putStrLn $ show x
    Left e -> liftIO $ e

