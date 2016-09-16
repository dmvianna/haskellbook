
module LookMa where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

foo :: IO ()
foo = flip runReaderT 10 $ do
  liftIO $ putStrLn "Hello! I'm in `ReaderT Int IO ()`!"
  val <- ask :: ReaderT Int IO Int
  liftIO $ print val
