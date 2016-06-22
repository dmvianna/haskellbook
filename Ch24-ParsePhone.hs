
module ParsePhone where

import Control.Applicative
import Text.Trifecta

type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- optional (string "1-")
  _ <- optional (char '(')
  npa <- count 3 digit
  _ <- optional (char ')')
  _ <- optional (oneOf " -")
  exc <- count 3 digit
  _ <- optional (oneOf " -")
  ln <- count 4 digit
  eof
  return $ PhoneNumber (read npa) (read exc) (read ln)
