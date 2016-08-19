{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercises where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  liftIO $ putStrLn $ "hi: " ++ show r
  return $ r + 1
