
module IPv4 where

import Data.Word
import Test.Hspec
import Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

type Rem = Integer
type Bit = Integer
type Pos = Integer

data ParseState =
  ParseState {
    remainder :: Rem
  , bits :: [Bit]
  , posit :: Pos
  } deriving (Show)

spewBit :: ParseState -> ParseState
spewBit ps =
  let p' = (posit ps) - 1
      b = 2 ^ p' :: Pos
      (q, r) = (remainder ps) `quotRem` b
      xs = q : (bits ps)
  in ParseState {remainder = r, bits = xs, posit = p'}

spewPart :: ParseState -> [Bit]
spewPart ps =
  if posit ps == 0
  then reverse $ bits ps
  else spewPart $ spewBit ps

initState :: Pos -> Integer -> ParseState
initState p n = ParseState {remainder = n, bits = mempty, posit = p}

parseIPv4 :: Parser IPAddress
parseIPv4 = do
  n <- decimal
  _ <- char '.'
  n' <- decimal
  _ <- char '.'
  sn <- decimal
  _ <- char '.'
  h <- decimal
  let xs = concat $ spewPart <$> initState 8 <$> [n,n',sn,h]
  return $ IPAddress (fromInteger $ bitToInteger (0, xs))

spewInteger :: (Integer, [Bit]) -> (Integer, [Bit])
spewInteger (s, xs) =
  let l = length xs - 1
      t = 2 ^ l
  in (s + head xs * t, tail xs)

bitToInteger :: (Integer, [Bit]) -> Integer
bitToInteger (i, xs) =
  if null xs
  then fromInteger i
  else bitToInteger $ spewInteger (i, xs)

main :: IO ()
main = hspec $ do

    describe "Test some IP values" $ do
          let ip1 = "172.16.254.1"
              ip2 = "204.120.0.15"
          it ("can parse " ++ ip1) $ do
            -- Nonexhaustive, I know
            let (Success x) = parseString parseIPv4 mempty ip1
            x `shouldBe` IPAddress 2886794753
    
          it ("can parse " ++ ip2) $ do
            let (Success x) = parseString parseIPv4 mempty ip2
            x `shouldBe` IPAddress 3430416399
