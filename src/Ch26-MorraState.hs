
module GameState where

-- import Control.Monad (replicateM_)
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.Word8
-- import Data.Bifunctor
-- import Data.IORef
-- import System.Exit
-- import System.IO
-- import System.Random

data Player = A | B deriving Show

data Value = Odd | Even deriving (Eq, Show)

data Turn = Turn { turnA :: Value
                 , turnB :: Value }
          deriving Show

data Score = Score { scoreA :: Word8
                   , scoreB :: Word8 }
           deriving Show

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
