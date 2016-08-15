{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MonadTrans where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad (liftM)

newtype IdentityT m a =
    IdentityT { runIdentityT :: m a }
      deriving (Eq, Show)

instance MonadTrans IdentityT where
  lift = IdentityT

instance (MonadIO m
         , Monad (IdentityT m))
         => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO


newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance (MonadIO m
         , Monad (MaybeT m))
         => MonadIO (MaybeT m) where
  liftIO = lift . liftIO


newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m
         , Monad (ReaderT r m))
         => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO


newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance (MonadIO m
         , Monad (EitherT e m))
         => MonadIO (EitherT e m) where
  liftIO = lift . liftIO


newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance (MonadIO m
         , Monad (StateT s m))
         => MonadIO (StateT s m) where
  liftIO = lift . liftIO
