{-# LANGUAGE TupleSections #-}

module MonadTrans where

import Control.Monad.Trans.Class
import Control.Monad (liftM)

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
      deriving (Eq, Show)

instance MonadTrans IdentityT where
  lift = IdentityT


newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just


newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const


newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right


newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
