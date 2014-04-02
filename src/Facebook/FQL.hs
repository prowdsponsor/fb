{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Facebook.FQL
    ( fqlQuery
    , FQLTime(..)
    , FQLList(..)
    , FQLObject(..)
    ) where

import Control.Applicative((<$>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HMS

import Facebook.Types
import Facebook.Monad
import Facebook.Base
import Facebook.Graph
import Facebook.Pager


-- | Query the Facebook Graph using FQL.
fqlQuery :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
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

{-# DEPRECATED FQLTime "Deprecated since fb 0.14.7, please use FbUTCTime instead." #-}


-- | @newtype@ wrapper around lists that works around FQL's
-- strange lists.
--
-- For example, if you fetch the @tagged_uids@ field from
-- @location_post@, you'll find that Facebook's FQL represents an
-- empty list of tagged UIDs as plain JSON array (@[]@).
-- However, it represents a singleton list as an object
-- @{\"1234\": 1234}@ instead of the much more correct @[1234]@.
--
-- On the other hand, not all FQL arrays are represented in this
-- bogus manner.  Also, some so-called arrays by FQL's
-- documentation are actually objects, see 'FQLObject'.
newtype FQLList a = FQLList { unFQLList :: [a] }
  deriving (Eq, Ord, Show)

instance A.FromJSON a => A.FromJSON (FQLList a) where
  parseJSON (A.Object o) = FQLList <$> mapM A.parseJSON (HMS.elems o)
  parseJSON v            = FQLList <$> A.parseJSON v


-- | @newtype@ wrapper around any object that works around FQL's
-- strange objects.
--
-- For example, if you fetch the @app_data@ field from @stream@,
-- you'll find that empty objects are actually represented as
-- empty lists @[]@ instead of a proper empty object @{}@.  Also
-- note that FQL's documentation says that @app_data@ is an
-- array, which it clear is not.  See also 'FQLList'.
newtype FQLObject a = FQLObject { unFQLObject :: a }
  deriving (Eq, Ord, Show)

instance A.FromJSON a => A.FromJSON (FQLObject a) where
  parseJSON (A.Array a) | a == mempty = FQLObject <$> A.parseJSON (A.Object mempty)
  parseJSON v                         = FQLObject <$> A.parseJSON v
