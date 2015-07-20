{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdCreative where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics

import Facebook.Graph
import Facebook.Monad
import Facebook.Types
import Facebook.Pager

-- | A Facebook page (see
-- <https://developers.facebook.com/docs/marketing-api/adcreative>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.

-- | Not an exhaustive list
data AdTypes = LinkAd | PageLikeAd | EventAd | PhotoAd
               deriving (Eq, Ord, Show, Read, Generic)

instance A.FromJSON AdTypes
instance A.ToJSON AdTypes

data CallToAction = OpenLink | BookTravel | ShopNow | PlayGame |
                    ListenMusic | WatchVideo | UseApp
                    deriving (Eq, Ord, Show, Read, Generic)

instance A.FromJSON CallToAction
instance A.ToJSON CallToAction

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

instance A.FromJSON RunStatus
instance A.ToJSON RunStatus

data AdCreative = AdCreative { ac_ad_actor_id             :: Id
                             , ac_ad_body                 :: Maybe Text
                             , ac_ad_follow_direct        :: Maybe Bool
                             , ac_ad_call_to_action_type  :: Maybe CallToAction
                             , ac_ad_image_crops          :: Maybe Bool
                             , ac_image_crops             :: Maybe A.Value
                             , ac_image_file              :: Maybe Text
                             , ac_image_hash              :: Maybe Text
                             , ac_image_url               :: Maybe Text
                             , ac_link_url                :: Maybe Text
                             , ac_name                    :: Maybe Text
                             , ac_object_id               :: Maybe Text
                             , ac_object_story_id         :: Maybe Text
                             , ac_object_story_spec       :: Maybe A.Value
                             , ac_object_url              :: Maybe Text
                             , ac_run_status              :: Maybe RunStatus
                             , ac_title                   :: Maybe Text
                             , ac_url_tags                :: Maybe Text
                             } deriving (Eq, Show, Generic)
