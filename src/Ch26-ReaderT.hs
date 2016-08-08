{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReaderT where

import Data.Bifunctor
import Data.Biapplicative

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ pure (pure x)
  ReaderT frma <*> ReaderT rma =
    ReaderT $ (<*>) <$> frma <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  ReaderT rma >>= f =
    ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r


newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT sma) = StateT $
    \s -> let r = sma s
          in first f <$> r

instance (Applicative m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  
  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b

  StateT fma <*> StateT ma = StateT $
    \s -> let -- r :: m (a, s)
              r = ma s
              -- fr :: m (a -> b, s)
              fr = fma s
              f' = \(f, _) (a, s') -> (f a, s')
          in f' <$> fr <*> r

-- instance (Monad m) => Monad (StateT s m) where
--   return = pure
--   (>>=) :: StateT s m a
--         -> (a -> StateT s m b)
--         -> StateT s m b
  -- StateT smb >>= f = StateT $
  -- \s -> let asmb = f s
