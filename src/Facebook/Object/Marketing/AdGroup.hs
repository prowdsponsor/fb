{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdGroup where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.Checkin
import Facebook.Object.Marketing.AdUser (AdUser)

import Facebook.Object.Marketing.Utility


-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/marketing-api/adgroup/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.

data AdGroupStatus = Active
                   | Paused
                   | PendingReview
                    deriving (Eq, Show, Typeable, Generic)

data ExeceutionOption = ValidateOnly
                        deriving (Eq, Show, Typeable, Generic)

data SocialPreference = AllowShares
                      | None
                        deriving (Eq, Show, Typeable, Generic)

data AdGroup = AdGroup
  { ag_name              :: Maybe Text
  , ag_adgroup_status    :: Maybe Text
  , ag_bid_amount        :: Int
  , ag_campaign_id       :: Integer
  , ag_creative            :: A.Value
  , ag_engagement_audience :: Bool
  , ag_execution_options    :: [ExeceutionOption]
  , ag_redownloadb         :: Bool
  , ag_tracking_specs      :: A.Value
  , ag_social_prefs        :: [SocialPreference]
  } deriving (Eq, Show, Typeable, Generic)
