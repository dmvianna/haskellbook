
module Ipv4 where

import Data.Word
import Test.Hspec
import Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

type Rem = Integer
type Bit = Integer
type Pos = Integer

spewBit :: (Rem, [Bit], Pos) -> (Rem, [Bit], Pos)
spewBit (i, xs, p) =
  let p' = p - 1
      b = 2 ^ p'
      (q, r) = i `quotRem` b
  in (r, q:xs, p')

spewPart :: (Rem, [Bit], Pos) -> [Bit]
spewPart (r,bs,p) =
  if p == 0
  then reverse bs
  else spewPart $ spewBit (r,bs,p)

initState :: Pos -> Integer -> (Rem, [Bit], Pos)
initState p n = (n, [], p)

parseIPv4 :: Parser [Bit]
parseIPv4 = do
  n <- decimal
  _ <- char '.'
  n' <- decimal
  _ <- char '.'
  sn <- decimal
  _ <- char '.'
  h <- decimal
  let xs = concat $ spewPart <$> initState 8 <$> [n,n',sn,h]
  return xs

spewInteger :: (Integer, [Bit]) -> (Integer, [Bit])
spewInteger (s, xs) =
  let l = length xs - 1
      t = 2 ^ l
  in (s + head xs * t, tail xs)

bitToInteger :: (Integer, [Bit]) -> Word32
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
          r = bitToInteger (0, x)
      IPAddress r `shouldBe` IPAddress 2886794753

    it ("can parse " ++ ip2) $ do
      let (Success x) = parseString parseIPv4 mempty ip2
          r = bitToInteger (0, x)
      IPAddress r `shouldBe` IPAddress 3430416399

