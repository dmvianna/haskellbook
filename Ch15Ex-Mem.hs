
import Data.Monoid (Monoid, mempty, mappend, Sum(..), getSum)
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck (Arbitrary,
                        arbitrary,
                        elements,
                        Gen,
                        quickCheck)

-- 9. Mem

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
    }

instance Monoid a => Monoid (Mem s a) where
  mempty = undefined
  mappend = undefined
