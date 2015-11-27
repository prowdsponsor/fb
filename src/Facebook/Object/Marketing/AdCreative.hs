{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdCreative where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Resource as R
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Control.Monad.IO.Class

import Facebook.Graph
import Facebook.Monad
import Facebook.Types
import Facebook.Pager

import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.Utility

-- | A Facebook page (see
-- <https://developers.facebook.com/docs/marketing-api/adcreative>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.

-- | Not an exhaustive list
data AdTypes = LinkAd | PageLikeAd | EventAd | PhotoAd
               deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON AdTypes
instance ToJSON AdTypes

data CallToAction = OpenLink | BookTravel | ShopNow | PlayGame |
                    ListenMusic | WatchVideo | UseApp
                    deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON CallToAction
instance ToJSON CallToAction

data RunStatus = Pending
               | Active
               | Paused
               | Deleted
               | PendingRevivew
               | Disapproved
               | Preapproved
               | PendingBillingInfo
               | CampaignPaused
               | AdgroupPaused
               | CampaignGroupPaused
               | Archived
                 deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RunStatus
instance ToJSON RunStatus

data AdCreative = AdCreative { ac_ad_actor_id             :: Maybe Id
                             , ac_ad_body                 :: Maybe Text
                             , ac_ad_follow_direct        :: Maybe Bool
                             , ac_ad_call_to_action_type  :: Maybe CallToAction
                             , ac_ad_image_crops          :: Maybe Bool
                             , ac_image_crops             :: Maybe Value
                             , ac_image_file              :: Maybe Text
                             , ac_image_hash              :: Maybe Text
                             , ac_image_url               :: Maybe Text
                             , ac_thumbnail_url           :: Maybe Text
                             , ac_link_url                :: Maybe Text
                             , ac_name                    :: Maybe Text
                             , ac_object_id               :: Maybe Text
                             , ac_object_story_id         :: Maybe Text
                             , ac_object_story_spec       :: Maybe Value
                             , ac_object_url              :: Maybe Text
                             , ac_run_status              :: Maybe RunStatus
                             , ac_title                   :: Maybe Text
                             , ac_url_tags                :: Maybe Text
                             , ac_id                      :: Text
                             } deriving (Eq, Show, Generic)


instance FromJSON AdCreative where
  parseJSON = parseJSONWithPrefix "ac_"

instance ToJSON AdCreative where
  toJSON = toJSONWithPrefix "ac_"


getAdCreatives :: (R.MonadResource m, MonadBaseControl IO m)  =>
                     Id
                  -> [Argument]
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager AdCreative)
getAdCreatives (Id id_) query tok = getObject ("/v2.5/" <> id_ <> "/adcreatives") query (Just tok)
