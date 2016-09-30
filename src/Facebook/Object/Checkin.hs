{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Object.Checkin
   ( Checkin(..)
   , CheckinFrom(..)
   , getCheckin
   , createCheckin
   ) where

import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A


import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager


-- | A Facebook check-in (see
-- <https://developers.facebook.com/docs/reference/api/checkin/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.
data Checkin =
    Checkin { checkinId          :: Id
            , checkinFrom        :: Maybe CheckinFrom
            , checkinPlace       :: Maybe Place
            , checkinCreatedTime :: Maybe UTCTime
            , checkinTags        :: Maybe (Pager Tag)
            , checkinMessage     :: Maybe Text
            }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Checkin where
    parseJSON (A.Object v) =
      Checkin <$> v .:  "id"
              <*> v .:? "from"
              <*> v .:? "place"
              <*> ((unFbUTCTime <$>) <$> v .:? "created_time")
              <*> v .:? "tags"
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
getCheckin :: (R.MonadResource m, MonadBaseControl IO m) =>
              Id                    -- ^ Checkin ID.
           -> [Argument]            -- ^ Arguments to be passed to Facebook.
           -> Maybe UserAccessToken -- ^ Optional user access token.
           -> FacebookT anyAuth m Checkin
getCheckin id_ query mtoken = getObject ("/" <> idCode id_) query mtoken


-- | Creates a 'check-in' and returns its ID. Place and
-- coordinates are both required by Facebook.
createCheckin :: (R.MonadResource m, MonadBaseControl IO m)  =>
                 Id               -- ^ Place ID.
              -> GeoCoordinates   -- ^ Coordinates.
              -> [Argument]       -- ^ Other arguments of the action.
              -> UserAccessToken  -- ^ Required user access token.
              -> FacebookT Auth m Id
createCheckin pid coords args usertoken = do
  let body = ("place" #= pid) : ("coordinates" #= coords) : args
  postObject "me/checkins" body usertoken
