{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module LogFile where

import Control.Applicative
import Data.ByteString hiding (foldr, count)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Test.Hspec
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
08:00 Breakfast -- nice
08:30 Shower -- the water was freezing!  
|]

parseActivity :: Parser String
parseActivity = do
  a <- try
                (manyTill (noneOf "\n") comment)
                <|> try (manyTill anyChar newline)
                <|> many anyChar

  skipOptional skipLine
  return a

comment :: Parser String
comment = try (someSpace >> string "--")
          <|> string "--"

skipLine :: Parser ()
skipLine = skipMany (noneOf "\n") >> skipOptional (char '\n') >> return ()

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


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do

         describe "Entry Parsing" $ do
               it "can parse a simple Entry" $ do
                 let m = parseByteString parseEntry
                         mempty "12:30 Lunch"
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (Entry (Time 12 30) "Lunch")

               it "can parse an Entry with comments -- no space" $ do
                 let m = parseByteString parseEntry
                         mempty "12:30 Lunch--felt full"
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (Entry (Time 12 30) "Lunch")

               it "can parse an Entry with comments -- one space" $ do
                 let m = parseByteString parseEntry
                         mempty "12:30 Lunch --felt full"
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (Entry (Time 12 30) "Lunch")

               it "can parse an Entry with comments -- many spaces" $ do
                 let m = parseByteString parseEntry
                         mempty "12:30 Lunch   --felt full\n"
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (Entry (Time 12 30) "Lunch")

         describe "Section Parsing" $ do
               it "can parse a simple section" $ do
                 let m = parseByteString parseSection
                         mempty miniLog
                     r' = maybeSuccess m
                 print m
                 r' `shouldBe` Just (Section (Date 2025 2 5)
                                    (M.fromList [(Time 8 0,"Breakfast")
                                                ,(Time 8 30,"Shower")]))
