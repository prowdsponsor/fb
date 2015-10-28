{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdCampaign where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text, pack, stripPrefix)
import Data.Typeable (Typeable)
import GHC.Generics

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Time -- Check if FbUTCTime is used

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.Marketing.AdAccount
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.Utility

data CampaignObjective = None
                       | OfferClaims
                       | PageLikes
                       | Canvas_app_installs
                       | EventResponses
                       | CanvasAppEngagement
                       | PostEngagement
                       | WebsiteConversions
                       | MobileAppInstalls
                       | WebsiteClicks
                       | MobileAppEngagement
                       | VideoViews
                       | MediaDownloads
                       | LocalAwareness
                       | InstagramBrandAwareness
                       | ProductCatalogSales
                       | XplatformSales
                       | LeadGeneration
                         deriving (Show, Generic)

data CampaignStatus = Active
                    | Paused
                    | Archived
                    | Deleted
                      deriving (Show, Generic)

instance A.FromJSON CampaignStatus where
  parseJSON = parseJSONPascal

instance A.ToJSON CampaignStatus where
  toJSON = toJSONPascal

data AdCampaign = AdCampaign
  { acamp_id :: Id
  , acamp_account_id :: Maybe AdAccountId
  , acamp_buying_type :: Maybe Text
  , acamp_campaign_group_status :: Maybe CampaignStatus
  , acamp_can_use_spend_cap :: Maybe Bool
  , acamp_created_time :: Maybe UTCTime
  , acamp_name :: Maybe Text
  , acamp_objective :: Maybe Integer
  , acamp_start_time :: Maybe UTCTime
  , acamp_stop_time :: Maybe UTCTime
  , acamp_topline_id :: Maybe Text
  , acamp_updated_time :: Maybe UTCTime
  , acamp_spend_cap :: Maybe Money
  } deriving (Show, Generic)

instance A.FromJSON AdCampaign where
  parseJSON = parseJSONWithPrefix "acamp_"

instance A.ToJSON AdCampaign where
  toJSON = toJSONWithPrefix "acamp_"

instance SimpleType AdCampaign where
  encodeFbParam = toBS . A.toJSON

createCampaign :: (R.MonadResource m, MonadBaseControl IO m)  =>
                 AdAccountId  -- ^ Ad Account ID.
              -> AdCampaign   -- ^ Ad Campaign.
              -> UserAccessToken  -- ^ Required user access token.
              -> FacebookT Auth m FBMObjectCreated
createCampaign aid campaign usertoken = do
  postObject ("/" <> toFbText aid  <>"/adcampaign_groups") [("campaign" #= campaign)] usertoken

getAccountCampaigns :: (R.MonadResource m, MonadBaseControl IO m)  =>
                       AdAccountId  -- ^ Ad Account ID.
                    -> [Argument]
                    -> UserAccessToken
                    -> FacebookT Auth m (Pager AdCampaign)
getAccountCampaigns id_ query tok = getObject ("/v2.5/" <> toFbText id_ <> "/campaigns") query (Just tok)
