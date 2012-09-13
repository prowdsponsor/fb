{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Facebook.FQL
    ( fqlQuery
    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Conduit as C

import Facebook.Types
import Facebook.Monad
import Facebook.Base
import Facebook.Graph
import Facebook.OpenGraph


-- | Query the Facebook Graph using FQL.
fqlQuery :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
            Text                        -- ^ FQL Query
         -> Maybe (AccessToken anyKind) -- ^ Optional access token
         -> FacebookT anyAuth m (Pager a)
fqlQuery fql mtoken =
  runResourceInFb $ do
    let query = ["q" #= fql]
    asJson =<< fbhttp =<< fbreq "/fql" mtoken query
