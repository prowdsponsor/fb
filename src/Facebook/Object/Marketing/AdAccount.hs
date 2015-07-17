{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdAccount where

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
import Facebook.Object.Checkin
import Facebook.Object.Marketing.AdUser (AdUser)

import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.Utility

data AccountGroup =
  AccountGroup
    { accg_account_group_id :: Id
    , accg_name :: Text
    , accg_status :: Text
    } deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON AccountGroup where
  parseJSON = parseJSONWithPrefix "ag_"

instance A.ToJSON AccountGroup where
  toJSON = toJSONWithPrefix "ag_"

data TaxStatus = Unknown
               | VATNotRequiredUSOrCA
               | VATNotRequired
               | VATSubmitted
               | OfflineVATFailed
               | Personal
                 deriving (Eq, Ord, Enum, Show, Read, Typeable, Generic)

instance A.FromJSON TaxStatus where
  parseJSON = parseJSONCamel

instance A.ToJSON TaxStatus where
  toJSON = toJSONCamel


data AccountStatus = Active | Disabled | Unsettled | PendingReview |
                     InGracePeriod | TemporarilyUnavailable |
                     PendingClosure
                     deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON AccountStatus where
  parseJSON (A.Number 1) = return Active
  parseJSON (A.Number 2) = return Disabled
  parseJSON (A.Number 3) = return Unsettled
  parseJSON (A.Number 7) = return PendingReview
  parseJSON (A.Number 9) = return InGracePeriod
  parseJSON (A.Number 101) = return TemporarilyUnavailable
  parseJSON (A.Number 100) = return PendingClosure
  parseJSON _ = mzero

instance A.ToJSON AccountStatus where
  toJSON Active = A.Number 1
  toJSON Disabled = A.Number 2
  toJSON Unsettled = A.Number 3
  toJSON PendingReview = A.Number 7
  toJSON InGracePeriod = A.Number 9
  toJSON TemporarilyUnavailable = A.Number 101
  toJSON PendingClosure = A.Number 100

-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/marketing-api/adaccount/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.

data Capabilities = BulkAccount
                  | CanCreateLookalikesWithCustomRatio
                  | CanUseConversionLookalikes
                  | CanUseMobileExternalPageType
                  | CanUseMobileExternalPageTypeForLPP
                  | CanUseReachAndFrequency
                  | Custom_cluster_sharing
                  | DirectSales
                  | HasAvailablePaymentMethods
                  | HoldoutViewTags
                  | Premium
                  | ViewTags
                  deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON Capabilities where
  parseJSON = parseJSONCamel

instance A.ToJSON Capabilities where
  toJSON = toJSONCamel

data FundingSource = Id
                   | Coupon
                   | Amount
                   | Currency
                   | DisplayAmount
                   | Expiration
                   | DisplayString
                   | Type
                   deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON FundingSource where
 parseJSON = parseJSONCamel

instance A.ToJSON FundingSource where
 toJSON = toJSONCamel

newtype AdAccountId =
  AdAccountId {unAdAcccountId :: Integer}
  deriving (Eq, Show)

class ToFbText a where
  toFbText :: a -> Text

instance ToFbText AdAccountId where
  toFbText (AdAccountId id)= "act_" <> (pack $ show id)

instance A.ToJSON AdAccountId where
  toJSON =  A.String . toFbText

instance A.FromJSON AdAccountId where
  parseJSON (A.String t) =
    case stripPrefix "act_" t of
      Nothing -> mzero
      Just idTxt ->
        let idParser = A.parseJSON (A.String idTxt)
        in AdAccountId <$> (idParser :: A.Parser Integer)
  parseJSON _ = mzero

data AdAccount =
    AdAccount {
           aa_account_groups             :: [AccountGroup]
         , aa_account_id                 :: AdAccountId
         , aa_account_status             :: Maybe AccountStatus
         , aa_age                        :: Maybe Float
         , aa_agency_client_declaraion   :: Maybe A.Object
         , aa_account_spend              :: Maybe Money
         , aa_balance                    :: Maybe Money
         , aa_business                   :: Maybe A.Value
         , aa_business_city              :: Maybe Text
         , aa_business_country_code      :: Maybe Text
         , aa_business_name              :: Maybe Text
         , aa_business_state             :: Maybe Text
         , aa_business_street            :: Maybe Text
         , aa_business_street2           :: Maybe Text
         , aa_business_zip               :: Maybe Text
         , aa_capabilities               :: Maybe [Capabilities]
         , aa_created_time               :: Maybe UTCTime
         , aa_currency                   :: Maybe Text
         , aa_end_advertiser             :: Maybe Integer
         , aa_funding_source             :: Maybe Id
         , aa_funding_source_details     :: Maybe Text
         , aa_id                         :: Maybe Text
         , aa_is_personal                :: Maybe Int
         , aa_media_agency               :: Maybe Integer
         , aa_name                       :: Maybe Text
         , aa_offsite_pixels_tos_accepted:: Maybe Bool
         , aa_partner                    :: Maybe Integer
         , aa_rf_spec                    :: Maybe A.Value
         , aa_spend_cap                  :: Maybe Money
         , aa_tax_id_status              :: Maybe TaxStatus
         , aa_timezone_id                :: Maybe Int
         , aa_timezone_name              :: Maybe Text
         , aa_timezone_offset_hours_utc  :: Maybe Int
         , aa_tos_accepted               :: Maybe A.Value
         , aa_users                      :: [AdUser]
         } deriving (Eq, Show, Typeable, Generic)

instance A.FromJSON AdAccount
instance A.ToJSON AdAccount

getAdAccount :: (R.MonadResource m, MonadBaseControl IO m) =>
           AdAccountId    -- ^ Ad Account Id
        -> [Argument]     -- ^ Arguments to be passed to Facebook.
        -> Maybe UserAccessToken -- ^ Optional user access token.
        -> FacebookT anyAuth m AdAccount
getAdAccount id_ query mtoken = getObject ("/" <> toFbText id_) query mtoken
