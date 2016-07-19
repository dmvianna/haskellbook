
module IPv6 where

import Data.List (elemIndex)
import Data.Maybe
import Data.Word
import Numeric
import Test.Hspec
import Text.Trifecta
import IPv4 hiding (main)

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)

instance Show IPAddress6 where
  show = show <$> ip6ToInteger

ip6ToInteger :: IPAddress6 -> Integer
ip6ToInteger (IPAddress6 q r) =
  (toInteger q)
  * (toInteger (maxBound :: Word))
  + (toInteger r)

hex :: [Char]
hex = ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']

parseBlock :: Parser [Char]
parseBlock = many $ oneOf hex

parseBlocks :: Parser [String]
parseBlocks = sepBy1 parseBlock (char ':')

fillAbbrev :: [String] -> [String]
fillAbbrev xs = -- I'm sure this can be shortened
  if "" `elem` xs
  then
    if length xs > 8
    then catMaybes $ (\a -> if a == "" then Nothing else Just a) <$> xs
    else let i = fromJust $ elemIndex "" xs
             t = splitAt i xs
         in fillAbbrev (fst t ++ ["0"] ++ snd t)
  else xs

parseIPv6 :: Parser IPAddress6
parseIPv6 = do
  xs <- parseBlocks
  let xs' = concat $ spewPart
          <$> initState 16
          <$> fst
          <$> (catMaybes $ listToMaybe
               <$> readHex
               <$> fillAbbrev xs)
      x = bitToIntegral (0, xs')
      ip = quotRem x (fromIntegral (maxBound :: Word))
  return $ IPAddress6 (fromIntegral $ fst ip) (fromIntegral $ snd ip)

main :: IO ()
main = hspec $ do

  describe "Test some IP values" $ do
    let ip1 = "FE80::0202:B3FF:FE1E:8329"
        ip2 = "0:0:0:0:0:ffff:cc78:f"
    it ("can parse " ++ ip1) $ do
      -- Nonexhaustive, I know
      let (Success x) = parseString parseIPv6 mempty ip1
          r = ip6ToInteger x
      r `shouldBe` 338288524927261089654163772891438416681

    it ("can parse " ++ ip2) $ do
      let (Success x) = parseString parseIPv6 mempty ip2
          r = ip6ToInteger x
      r `shouldBe` 281474112159759

