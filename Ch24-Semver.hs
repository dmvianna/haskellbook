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

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> try (decimal <* notFollowedBy letter))
           <|> (NOSS <$> some (letter <|> digit))

parsePrerelease :: Parser NumberOrString
parsePrerelease = skipMany (oneOf ".") >> parseNOS

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  _ <- char '-'
  vrelease <- some parsePrerelease
  _ <- char '+'
  metadata <- some parsePrerelease
  return $ SemVer major minor patch vrelease metadata

