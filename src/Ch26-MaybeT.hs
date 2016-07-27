
module MaybeT where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

-- instance (Functor f, Functor g) =>
--          Functor (Compose f g) where
--            fmap f (Compose fga) =
--              Compose $ (fmap . fmap) f fga

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ fab <*> mma

