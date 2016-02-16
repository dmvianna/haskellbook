
import Test.QuickCheck

divGen :: Gen (Maybe Float)
divGen = do
  x <- arbitrary
  if x /= 0
  then return $ Just x
  else return Nothing
