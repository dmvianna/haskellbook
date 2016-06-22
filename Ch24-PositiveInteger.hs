
module PositiveInteger where

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"
  

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit
