
module PositiveInteger where

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf "0123456789" <?> "a digit between 0 and 9"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  sign <- optional (char '-')
  xs <- some parseDigit
  case sign of
    Nothing -> return (read xs)
    Just x -> return (read $ x:xs)
