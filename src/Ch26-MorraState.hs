
module MorraState where

-- import Control.Monad (replicateM_)
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.Word8
-- import Data.Bifunctor
-- import Data.IORef
-- import System.Exit
-- import System.IO
import System.Random

data Player = A | B deriving Show

data Guess = Odd | Even deriving (Bounded, Enum, Eq, Show)

data Turn = Turn { turnA :: Guess
                 , turnB :: Guess }
          deriving Show

data Score = Score { scoreA :: Word8
                   , scoreB :: Word8 }
           deriving Show

-- Scores

updateScore :: Player -> Score -> Score
updateScore p s =
  case p of
    A -> Score {scoreA = scoreA s + 1, scoreB = scoreB s}
    B -> Score {scoreA = scoreA s, scoreB = scoreB s + 1}

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

ai :: [Turn] -> IO Guess
ai ts =
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


-- play :: Turn -> StateT Score IO ()
-- play t = do
--   ts <- get
--   put $ t:ts
--   return $ getScore t:ts

-- addTurn :: Score -> State [Turn] Score
-- addTurn x = do
--   xs <- get
--   put $ x:xs
--   return $ updateScore 
