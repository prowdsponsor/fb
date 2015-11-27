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
  { as_optimization_goal       :: Maybe UserEvent
  , as_bid_amount              :: Maybe Money
  , as_campaign_group_id       :: Maybe FbNumeric
  , as_campaign_schedule       :: Maybe A.Value
  , as_configured_status       :: Maybe CampaignStatus
  , as_effective_status       :: Maybe CampaignStatus
  , as_creative_sequence       :: Maybe [FbNumeric]
  , as_daily_budget            :: Maybe Money
  , as_daily_imps              :: Maybe Integer
  , as_date_format             :: Maybe Text
  , as_end_time                :: Maybe UTCTime
  , as_execution_options       :: Maybe ExecutionOption
  , as_io_number               :: Maybe Integer
  , as_is_autobid              :: Maybe Bool
  , as_lifetime_budget         :: Maybe Integer
  , as_lifetime_frequency_cap  :: Maybe Integer
  , as_lifetime_imps           :: Maybe Integer
  , as_name                    :: Maybe Text
  , as_redownload              :: Maybe Bool
  , as_rf_prediction_id        :: Maybe FbNumeric
  , as_start_time              :: Maybe UTCTime
  , as_targeting               :: Maybe A.Value
  , as_time_start              :: Maybe UTCTime
  , as_time_stop               :: Maybe UTCTime
  , as_promoted_object         :: Maybe A.Value
  , as_billing_event           :: Maybe UserEvent
  , as_product_ad_behavior     :: Maybe AdBehaviour
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

getCampaignAdSets :: (R.MonadResource m, MonadBaseControl IO m)  =>
                     Id
                  -> [Argument]
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager AdSet)
getCampaignAdSets (Id id_) query tok = getObject ("/v2.5/" <> id_ <> "/adsets") query (Just tok)

setDailyBudget :: (R.MonadResource m, MonadBaseControl IO m)  =>
                     Id
                  -> Int
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager AdSet)
setDailyBudget (Id id_) amount tok = postObject ("/v2.5/" <> id_ ) [("daily_budget" #= amount)] tok
