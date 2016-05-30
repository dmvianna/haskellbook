
module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

-- char :: Char -> Parser Char
-- char c =
--   Parser $ \s ->
--       case s of
--         (x:xs) -> if c == x
--                   then [(c, xs)]
--                   else []
--         _ -> []

-- type Token = Char

-- newtype Parser a = P ([Token] -> [(a, [Token])])

-- type Parser' a = String -> [(a, String)]


-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

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

