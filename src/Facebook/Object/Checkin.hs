{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Object.Checkin
   ( Checkin(..)
   , CheckinFrom(..)
   , getCheckin
   , createCheckin
   ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)

-- import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE (fromValue)
import qualified Data.Conduit as C
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB


import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.OpenGraph


-- | A Facebook check-in (see
-- <https://developers.facebook.com/docs/reference/api/checkin/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.
data Checkin =
    Checkin { checkinId :: Id
            , checkinFrom :: CheckinFrom
            , checkinCreatedTime :: Maybe UTCTime
            , checkinMessage :: Maybe Text
            }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Checkin where
    parseJSON (A.Object v) =
      Checkin <$> v .:  "id"
              <*> v .:  "from"
              <*> v .:? "created_time"
              <*> v .:? "message"
    parseJSON _ = mzero


-- | Information about the user who made the check-in.
data CheckinFrom =
    CheckinFrom { checkinFromId   :: UserId
                , checkinFromName :: Text
                }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON CheckinFrom where
  parseJSON (A.Object v) =
    CheckinFrom <$> v .: "id"
                <*> v .: "name"
  parseJSON _ = mzero


-- | Get a checkin from its ID.  The user access token is
-- optional, but when provided more information can be returned
-- back by Facebook.
getCheckin :: (C.MonadResource m, MonadBaseControl IO m) =>
              Id                    -- ^ Checkin ID.
           -> [Argument]            -- ^ Arguments to be passed to Facebook.
           -> Maybe UserAccessToken -- ^ Optional user access token.
           -> FacebookT anyAuth m Checkin
getCheckin (Id id_) query mtoken = getObject ("/" <> id_) query mtoken


-- | Creates a 'check-in' and returns its ID. Place and
-- coordinates are both required by Facebook.
createCheckin :: (C.MonadResource m, MonadBaseControl IO m)  =>
                 Id               -- ^ Place Id
              -> (Double, Double) -- ^ (Latitude, Longitude)
              -> [Argument]       -- ^ Other arguments of the action.
              -> UserAccessToken  -- ^ Required user access token.
              -> FacebookT Auth m Id
createCheckin pid (lat,lon) args usertoken = do
  let coords = ("coordinates", toBS $ A.object ["latitude" A..= lat, "longitude" A..= lon])
      body = ("place" #= pid) : coords : args
      toBS = TE.encodeUtf8 . TL.toStrict . TLB.toLazyText . AE.fromValue
  postObject "me/checkins" body usertoken
