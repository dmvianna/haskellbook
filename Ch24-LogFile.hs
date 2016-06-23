{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module LogFile where

import Control.Applicative
import Data.ByteString hiding (foldr, count)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Text.RawString.QQ
import Text.Trifecta

log :: ByteString
log = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

type Hours = Int
type Minutes = Int
type Day = Int
type Month = Int
type Year = Int
type Activity = String

data Date = Date Year Month Day deriving (Eq, Show)
data Time = Time Hours Minutes deriving (Eq, Show)
data Entry = Entry Time Activity deriving (Eq, Show)
data Section = Section Date (Map Time Activity) deriving (Eq, Show)

miniLog :: ByteString
miniLog = [r|
# 2025-02-05
08:00 Breakfast
|]

parseActivity :: Parser String
parseActivity = try (manyTill anyChar comment)
                <|> try (manyTill anyChar trailingSpace)
                <|> many anyChar

trailingSpace :: Parser String
trailingSpace = string " \n" <|> string "\n"

comment :: Parser String
comment = string " --" <|> string "--"

skipComment :: Parser ()
skipComment = comment >> skipMany (noneOf "\n")

parseDate :: Parser Date
parseDate = do
  _ <- string "# "
  year <- count 4 digit
  _ <- char '-'
  month <- count 2 digit
  _ <- char '-'
  day <- count 2 digit
  return $ Date (read year) (read month) (read day)

parseEntry :: Parser Entry
parseEntry = do
  h <- count 2 digit
  _ <- char ':'
  m <- count 2 digit
  _ <- char ' '
  e <- parseActivity
  return $ Entry (Time (read h) (read m)) e

parseSection :: Parser Section
parseSection = do
  whiteSpace
  d <- parseDate
  whiteSpace
  entries <- some parseEntry
  return $ Section d (M.fromList $ readEntry <$> entries)

readEntry :: Entry -> (Time, Activity)
readEntry (Entry t a) = (t, a)

instance Ord Time where
  Time h m `compare` Time h' m' =
    compare h h' <> compare m m'
