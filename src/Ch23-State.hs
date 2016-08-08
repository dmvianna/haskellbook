{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module Moi where

import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Moi s a = Moi { runMoi :: s -> (a, s) } deriving (Show, Generic)

instance Functor (Moi s) where
  fmap :: (a -> b)
       -> Moi s a
       -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, b) = g s
                               in (f a, b)
  

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (fab, s') = f s
                                        (a, s'') = g s'
                                    in (fab a, s'')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                                  (Moi sb) = g a
                              in sb s'


genMoi :: (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a)
          => Gen (s -> (a, s))
genMoi = arbitrary

instance (Arbitrary s, Arbitrary a, CoArbitrary s, CoArbitrary a)
         => Arbitrary (Moi s a) where
  arbitrary = do
    f <- genMoi
    return $ Moi f

instance (Eq a, Eq s) => EqProp (Moi s a) where
  (=-=) = eq


main :: IO ()
main = do
  let trigger = undefined :: Moi (Int -> (String, Int)) (Int, Int, [Int])
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
