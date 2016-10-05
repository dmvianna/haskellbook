
-- I know, it is a long file. Normally I would place it in a
-- self-contained project with its own Module hierarchy. But
-- I'm keeping it here with the other haskellbook exercises.

module MorraState where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Data.Word8
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

finalWinner :: Score -> Player
finalWinner s =
  if scoreA s > scoreB s
  then A
  else B

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
-- Strings

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
    putStrLn "Press 1 for odds, 2 for evens."
    putStrLn $ player m A ++ " is evens,"
    putStrLn $ player m B ++ " is odds."

-- Person input

promptInput :: Name -> IO Char
promptInput n = do
  putStr $ n ++ ": "
  c <- getChar
  _ <- getChar
  return c

parseInput :: Char -> ExceptT (IO ()) IO Guess
parseInput c
  | c `elem` "12" = return $ (toEnum . (subtract 1) . read) [c]
  | otherwise = throwE $ exceptHandler c

exceptHandler :: Char -> IO ()
exceptHandler c
  | c `elem` "Qq" = putStrLn "Quitting..." >> exitSuccess
  | otherwise = putStrLn "Press '1' for Odd, '2' for Even, and Q for Quit"

personGuess :: MonadIO m
               => Player
               -> ReaderT Mode m (Either (IO ()) Guess)
personGuess p = do
  m <- ask
  c <- liftIO $ promptInput $ player m p
  c' <- liftIO $ runExceptT $ parseInput c
  return c'

-- Modes

parseMode :: Char -> ExceptT (IO ()) IO Mode
parseMode c
  | c `elem` "Pp" = return $ P2P
  | c `elem` "Cc" = return $ AI2P
  | otherwise = throwE $
                (putStrLn $ "Key pressed: " ++ [c])
                >> putStrLn "Quitting..."

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

ai2p :: StateT [Turn] IO ()
ai2p = do
  ts <- get
  aig <- liftIO $ aiTurn ts
  pg' <- liftIO $ runReaderT (personGuess B) AI2P
  case pg' of
    Right pg -> do
      let turn = Turn aig pg
          w = player AI2P $ getWinner turn
          c = player AI2P A
      put $ turn:ts
      liftIO $ putStrLn $ c ++ ": " ++ show aig
      liftIO $ putStrLn $ "- " ++ w ++ " wins"
      ai2p
    Left e -> liftIO $ printScore AI2P ts
              >> e >> runStateT ai2p ts >> return ()

printScore :: Mode -> [Turn] -> IO ()
printScore m ts = do
  let score = getScore ts
  putStrLn $ "***** Score *****"
  putStrLn $ player m A ++ ": " ++ (show . scoreA) score
  putStrLn $ player m B ++ ": " ++ (show . scoreB) score
  putStrLn $ "Way to go, " ++ (player m $ finalWinner score) ++ "!"
  putStrLn $ "*****************"

p2pGuess :: Player -> IO Guess
p2pGuess p = do
  pg <- runReaderT (personGuess p) P2P
  case pg of
    Right g ->
      replicateM_ 12 (putStrLn "\n") >> return g -- poor man's blank screen
    Left e ->
      e >> p2pGuess p

p2p :: StateT [Turn] IO ()
p2p = do
  g'  <- liftIO $ p2pGuess A
  g'' <- liftIO $ p2pGuess B
  ts  <- get
  let turn = Turn g' g''
      ts' = turn:ts
  liftIO $ do
    putStrLn $ "- " ++ (player P2P $ getWinner turn) ++ " wins"
    printScore P2P ts'
  put ts'
  p2p

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  m'' <- promptMode
  m' <- runExceptT $ parseMode m''
  case m' of
    Right AI2P -> do
      runReaderT printRules AI2P
      runStateT ai2p [] >> return ()
    Right P2P -> do
      runReaderT printRules P2P
      runStateT p2p [] >> return ()
    Left e -> e
