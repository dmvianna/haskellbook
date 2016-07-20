{-# LANGUAGE InstanceSigs #-}

module Twinplicative where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
           fmap f (Compose fga) =
             Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
           pure :: a -> Compose f g a
           pure x = Compose $ (pure . pure) x

           (<*>) :: Compose f g (a -> b)
                 -> Compose f g a
                 -> Compose f g b
           (Compose f) <*> (Compose a) =
             Compose $ (fmap (id . (<*>)) f) <*> a

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
           foldMap f (Compose fga) =
             (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
           traverse :: Applicative f1 => (a -> f1 b)
                    -> Compose f g a
                    -> f1 (Compose f g b)
           traverse f (Compose fga) =
             Compose <$> (traverse . traverse) f fga
