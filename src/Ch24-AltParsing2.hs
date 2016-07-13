{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
  Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
           >> (Left <$> integer)
           <|> (Right <$> some letter)

main = do
  print $ parseString (some (token parseNos)) mempty eitherOr
