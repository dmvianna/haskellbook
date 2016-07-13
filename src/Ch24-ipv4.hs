
import Data.Word
import Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

f :: Int
f = undefined

--f

-- parse8Bits :: Parser Integer
-- parse8Bits = do
--   x <- decimal
--   n8 <- quot x 128
--   r8 <- rem x 128
--   n7 <- quot r8 64
--   r7 <- rem r8 64
--   n6 <- quot x 32
--   r6 <- rem r7 32
--   n5 <- quot r6 16
--   r5 <- rem r6 16
--   n4 <- quot r5 8
--   r4 <- rem r5 8
  
  

-- binRep :: Parser Word32
-- binRep = do
--   xs <- parse8Bits
  
