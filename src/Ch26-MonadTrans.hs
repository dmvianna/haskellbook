{-# LANGUAGE InstanceSigs        #-}

module MonadTrans where

import           Control.Monad

import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.Class (MonadTrans (lift))

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift ma = MaybeT $ fmap Just ma

instance MonadTrans (ReaderT r) where
  -- Equivalent to ReaderT const
  lift ma = ReaderT $ \r -> ma

instance MonadTrans (EitherT e) where
  -- Pointfree version of: lift ma = EitherT $ fmap Right ma
  lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma

instance MonadIO m => MonadIO (IdentityT m) where
  liftIO :: IO a -> IdentityT m a
  liftIO ioA = IdentityT ma
    -- Use m's liftIO to put ioA into m
    where
      ma = liftIO ioA

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  -- This is m's liftIO and EitherT's lift
  liftIO = lift . liftIO
-- Equivalent to:
--  liftIO ioA = EitherT $ fmap Right ma
--    where ma = liftIO ioA

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
-- Equivalent to:
--  liftIO ioA = MaybeT mMaybeA
--    where mMaybeA = fmap Just ma
--          ma = liftIO ioA

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO ioA = ReaderT rToMa
    where
      rToMa r = liftIO ioA

instance MonadIO m => MonadIO (StateT s m) where
  liftIO ioA = StateT smas
    where
      smas s = fmap (\a -> (a, s)) ma
      ma = liftIO ioA
