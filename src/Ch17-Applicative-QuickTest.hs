
module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Bull (Fails left and right identity)

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

-- applicative

-- applicative
--     :: (Show a, Show (m a), Show (m (a -> b)), Show (m (b -> c)),
--         Applicative m, CoArbitrary a, EqProp (m a), EqProp (m b),
--         EqProp (m c), Arbitrary a, Arbitrary b, Arbitrary (m a),
--         Arbitrary (m (a -> b)), Arbitrary (m (b -> c))) =>
--        m (a, b, b) -> TestBatch

main :: IO ()
main = quickBatch (monoid Twoo)

