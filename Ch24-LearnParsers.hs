{-# LANGUAGE RankNTypes #-}

module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
-- two = char '2'
-- three = char '3'

one' = one >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testEOF :: Parser () -> IO ()
testEOF p =
  print $ parseString p mempty "123"

-- string parsers

type S = forall m. CharParsing m => m String

oneS :: S
oneS = string "1"

oneTwoS :: S
oneTwoS = string "12"

oneTwoThreeS :: S
oneTwoThreeS = string "123"

testParse' :: Parser String -> IO ()
testParse' p =
  print $ parseString p mempty "123"

-- One Parser rules them all
-- how do we prevent >> to drop it on the floor?

--

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "one >> EOF:"
  testEOF (one >> eof)
  pNL "oneTwo >> EOF"
  testEOF (oneTwo >> eof)
  pNL "string \"1\", \"12\", \"123\""
  testParse' (choice [ oneTwoThreeS
                     , oneTwoS
                     , oneS
                     , stop ])
