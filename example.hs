{-# LANGUAGE OverloadedStrings #-}

import qualified Facebook as FB

import Network.HTTP.Conduit (withManager)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  withManager $ \manager -> do
    FB.runNoAuthFacebookT manager $ do
       u <- FB.getUser "zuck" [] Nothing
       liftIO $ print (FB.userName u)
