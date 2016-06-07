{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

type FractionOrDecimal =
  Either Rational Integer

eitherSuccess :: String
eitherSuccess = [r|
123
1/2
23
3/23
|]

eitherFail :: String
eitherFail = [r|
123
1/2
3/0
|]

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Integer
parseDecimal = do
  x <- decimal
  c <- anyChar
  case c of
    '\n' -> return x
    _ -> fail "Unexpected character"

parseEither :: Parser FractionOrDecimal
parseEither =
  skipMany (oneOf "\n")
           >> (Left <$> try parseFraction)
           <|> (Right <$> parseDecimal) -- I want errors to fail here
                                        -- 'try' will silence errors
                                        -- and let successes pass

main = do
  print $ parseString (some (token parseEither)) mempty eitherSuccess
  print $ parseString (some (token parseEither)) mempty eitherFail
