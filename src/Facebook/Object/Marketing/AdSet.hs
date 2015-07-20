{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdSet where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import GHC.Generics
import Data.Time

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager

import Facebook.Object.Marketing.AdAccount(AdAccountId)
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.Utility
import Facebook.Object.Marketing.AdCampaign

data ExecutionOption = ValidateOnly
                      | SynchronousAdReview
                        deriving (Show, Generic)

instance A.FromJSON ExecutionOption
instance A.ToJSON ExecutionOption

data UserEvent = AppInstalls
               | Clicks
               | Impressions
               | LinkClicks
               | None
               | OfferClaims
               | PageLikes
               | PostEngagement
                 deriving (Show, Generic)

instance A.FromJSON UserEvent
instance A.ToJSON UserEvent

data AdBehaviour = RequireLastSeenProducts
                  | RequireAvailableLastSeenProducts
                  | FallBackToFbRecommendations
                    deriving (Show, Generic)

instance A.FromJSON AdBehaviour
instance A.ToJSON AdBehaviour

data AdSet = AdSet
  { as_optimization_goal       :: UserEvent
  , as_bid_amount              :: Money
  , as_campaign_group_id       :: FbNumeric
  , as_campaign_schedule       :: A.Value
  , as_campaign_status         :: CampaignStatus
  , as_creative_sequence       :: [FbNumeric]
  , as_daily_budget            :: Money
  , as_daily_imps              :: Integer
  , as_date_format             :: Text
  , as_end_time                :: UTCTime
  , as_execution_options       :: ExecutionOption
  , as_io_number               :: Integer
  , as_is_autobid              :: Bool
  , as_lifetime_budget         :: Integer
  , as_lifetime_frequency_cap  :: Integer
  , as_lifetime_imps           :: Integer
  , as_name                    :: Text
  , as_redownload              :: Bool
  , as_rf_prediction_id        :: FbNumeric
  , as_start_time              :: UTCTime
  , as_targeting               :: A.Value
  , as_time_start              :: UTCTime
  , as_time_stop               :: UTCTime
  , as_promoted_object         :: Maybe A.Value
  , as_billing_event           :: UserEvent
  , as_product_ad_behavior     :: AdBehaviour
  , as_id :: Text
  } deriving (Show, Generic)

instance A.FromJSON AdSet where
  parseJSON = parseJSONWithPrefix "as_"

instance A.ToJSON AdSet where
  toJSON = toJSONWithPrefix "as_"

instance SimpleType AdSet where
  encodeFbParam = toBS . A.toJSON

createAdSet :: (R.MonadResource m, MonadBaseControl IO m)  =>
                 AdAccountId  -- ^ Ad Account ID.
              -> AdSet   -- ^ Ad AdSet.
              -> UserAccessToken  -- ^ Required user access token.
              -> FacebookT Auth m FBMObjectCreated
createAdSet aid adset usertoken = do
  postObject ("/" <> toFbText aid  <>"/adcampaigns") [("adset" #= adset)] usertoken
