{-# LANGUAGE InstanceSigs #-}

module ReaderT where

import Data.Bifunctor

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

