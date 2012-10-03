{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Facebook.FQL
    ( fqlQuery
    , FQLTime(..)
    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.Aeson as A
import qualified Data.Conduit as C

import Facebook.Types
import Facebook.Monad
import Facebook.Base
import Facebook.Graph


-- | Query the Facebook Graph using FQL.
fqlQuery :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
            Text                        -- ^ FQL Query
         -> Maybe (AccessToken anyKind) -- ^ Optional access token
         -> FacebookT anyAuth m (Pager a)
fqlQuery fql mtoken =
  runResourceInFb $ do
    let query = ["q" #= fql]
    asJson =<< fbhttp =<< fbreq "/fql" mtoken query


-- | @newtype@ wrapper around 'UTCTime' that is able to parse
-- FQL's time representation as seconds since the Unix epoch.
newtype FQLTime = FQLTime { unFQLTime :: UTCTime }
  deriving (Eq, Ord, Show)

instance A.FromJSON FQLTime where
  parseJSON = fmap ( FQLTime
                   . posixSecondsToUTCTime
                   . fromInteger)
            . A.parseJSON
