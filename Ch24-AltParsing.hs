
module AltParsing where

import Control.Applicative
import Text.Trifecta

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNOS :: Parser NumberOrString
parseNOS =
        (Left  <$> integer)
    <|> (Right <$> some letter)

main = do
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b
  print $ parseString parseNOS mempty a
  print $ parseString parseNOS mempty b
  print $ parseString (many parseNOS) mempty c
  print $ parseString (some parseNOS) mempty c
