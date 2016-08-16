{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Web.Scotty

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k = ExceptT $
           rescue (Right <$> param k)
           (const
            (return
             (Left $ "They key: "
              ++ show k
              ++ " was missing!")))

type Reco = (Integer, Integer, Integer, Integer)

tshow :: Reco -> Text
tshow = TL.pack . show

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    reco <- runExceptT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    case reco of
      (Left e) -> text (TL.pack e)
      (Right r) ->
        html $ mconcat ["<h1>Success! Reco was: ", tshow r, "</h1>"]
