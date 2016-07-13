{-# LANGUAGE OverloadedStrings #-}

module Semver where

import Control.Applicative
import Data.Monoid ((<>))
import Text.Trifecta


data NumberOrString = NOSS String
                    | NOSI Integer
                      deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
newtype Release = Release [NumberOrString] deriving (Show, Eq)
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
            deriving (Show, Eq)

ver :: String
ver = "1.0.0-x.7.z.92"

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> try (decimal <* notFollowedBy letter))
           <|> (NOSS <$> some (letter <|> digit))

parsePrerelease :: Parser NumberOrString
parsePrerelease = skipMany (oneOf ".") >> parseNOS

parseSemVer :: Parser SemVer
parseSemVer = SemVer
              <$> decimal
              <*> (char '.' *> decimal)
              <*> (char '.' *> decimal)
              <*> (Release <$> (char '-' *> some parsePrerelease <|> mempty))
              <*> (char '+' *> some parsePrerelease <|> mempty)

instance Ord NumberOrString where
  (NOSI _) `compare` (NOSS _) = GT
  (NOSS _) `compare` (NOSI _) = LT
  (NOSI x) `compare` (NOSI x') = x `compare` x'
  (NOSS x) `compare` (NOSS x') = x `compare` x'

instance Ord Release where
  (Release []) `compare` (Release []) = EQ
  (Release []) `compare` (Release _) = GT
  (Release _) `compare` (Release []) = LT
  (Release x) `compare` (Release x') = x `compare` x'

instance Ord SemVer where
    (SemVer mj mn p r _) `compare` (SemVer mj' mn' p' r' _) =
        compare mj mj'
                <> compare mn mn'
                <> compare p p'
                <> compare r r'

-- still need to test

-- This is all correct as per semver.org:
-- λ> SemVer 2 1 1 (Release [NOSI 1]) [] > SemVer 2 1 0 (Release []) []
-- True
-- λ> SemVer 2 1 0 (Release [NOSI 1]) [] > SemVer 2 1 0 (Release []) []
-- False
