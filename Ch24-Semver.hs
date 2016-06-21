{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Semver where

import Control.Applicative
import Data.String (IsString)
import Text.Trifecta


data NumberOrString = NOSS String
                    | NOSI Integer
                      deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
            deriving (Show, Eq)

ver :: String
ver = "10.0.0-x.7.z.92"

parseStop :: Parser a -> Parser a
parseStop p = do
  x <- p
  c <- anyChar
  if c `elem` (".-+" :: String)
  then return x
  else fail "Unexpected character"

parseEOF :: Parser a -> Parser a
parseEOF p = do
  x <- p
  eof
  return x

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> try (parseStop decimal))
           <|> (NOSI <$> try (parseEOF decimal))
           <|> (NOSS <$> some (letter <|> digit))

parsePrerelease :: Parser [NumberOrString]
parsePrerelease = undefined

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  _ <- char '-'
  vrelease <- parsePrerelease
  _ <- char '+'
  metadata <- parsePrerelease
  return $ SemVer major minor patch vrelease metadata

