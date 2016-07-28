{-# LANGUAGE InstanceSigs #-}

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
  MaybeT fab <*> MaybeT mma =
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  MaybeT ma >>= f =
    MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)


newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  EitherT fab <*> EitherT mma =
    EitherT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
  return = pure
  
  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  EitherT ma >>= f =
    EitherT $ do
    v <- ma
    case v of
      Left e -> return $ Left e
      Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither x =
  case x of
    Left e -> Right e
    Right a -> Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT x) = EitherT $ swapEither <$> x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fe _ (Left e) = fe e
either' _ fa (Right a) = fa a

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT x) = x >>= either fa fb
