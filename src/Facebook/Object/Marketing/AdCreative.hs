{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdCreative where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Typeable (Typeable)
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
               deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON AdTypes
instance A.ToJSON AdTypes

data CallToAction = OpenLink | BookTravel | ShopNow | PlayGame |
                    ListenMusic | WatchVideo | UseApp
                    deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON CallToAction
instance A.ToJSON CallToAction

data RunStatus = PENDING
               | ACTIVE
               | PAUSED
               | DELETED
               | PENDING_REVIVEW
               | DISAPPROVED
               | PREAPPROVED
               | PENDING_BILLING_INFO
               | CAMPAIGN_PAUSED
               | ADGROUP_PAUSED
               | CAMPAIGN_GROUP_PAUSED
               | ARCHIVED
                 deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON RunStatus
instance A.ToJSON RunStatus

data AdCreative = AdCreative { ad_actor_id             :: Id
                             , ad_body                 :: Maybe Text
                             , ad_follow_direct        :: Maybe Bool
                             , ad_call_to_action_type  :: Maybe CallToAction
                             , ad_image_crops          :: Maybe Bool
                             , image_crops             :: Maybe A.Value
                             , image_file              :: Maybe Text
                             , image_hash              :: Maybe Text
                             , image_url               :: Maybe Text
                             , link_url                :: Maybe Text
                             , name                    :: Maybe Text
                             , object_id               :: Maybe Text
                             , object_story_id         :: Maybe Text
                             , object_story_spec       :: Maybe A.Value
                             , object_url              :: Maybe Text
                             , run_status              :: Maybe RunStatus
                             , title                   :: Maybe Text
                             , url_tags                :: Maybe Text
                             } deriving (Eq, Show, Typeable, Generic)
