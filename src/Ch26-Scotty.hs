{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT
      . (ExceptT . fmap Right)
      . ReaderT . const
      . \m -> StateT (\s -> do
                         a <- m
                         return (a, s))
      ) (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!<h1>"]

