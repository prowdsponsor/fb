{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Facebook.Object.Marketing.AdAccountPlayground where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text, pack, stripPrefix, unpack)
import Data.Typeable (Typeable)
import GHC.Generics hiding ((:*:))

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Time -- Check if FbUTCTime is used

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.Marketing.AdUser (AdUser)
--import qualified Facebook.Object.Marketing.AdAccountGroup as AAG

import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.Utility
import Facebook.Records

import qualified Data.Vector as V
import qualified Facebook.Object.Marketing.AdAccountGroupPlayground as AAGP

data TaxStatus = Unknown
               | VATNotRequiredUSOrCA
               | VATNotRequired
               | VATSubmitted
               | OfflineVATFailed
               | Personal
                 deriving (Eq, Ord, Enum, Show, Read, Typeable, Generic)

instance A.FromJSON TaxStatus where
  parseJSON = parseJSONPascal

instance A.ToJSON TaxStatus where
  toJSON = toJSONPascal


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
  parseJSON = parseJSONPascal

instance A.ToJSON Capabilities where
  toJSON = toJSONPascal

newtype AdAccountId =
  AdAccountId {unAdAcccountId :: Integer}
  deriving (Eq, Show)

instance ToFbText AdAccountId where
  toFbText (AdAccountId aid)= "act_" <> pack (show aid)

instance A.ToJSON AdAccountId where
  toJSON =  A.String . toFbText

instance A.FromJSON AdAccountId where
  parseJSON (A.String t) =
    case stripPrefix "act_" t of
      Nothing -> mzero
      Just idTxt ->
        let idParser = return $ read $ unpack idTxt
        in AdAccountId <$> (idParser :: A.Parser Integer)

  parseJSON _ = mzero

data FId = FId
instance Field FId where
    type FieldValue FId = Id
    fieldName _ = "id"
    fieldLabel = FId

data FundingSourceDetails =
      FS_Id
    | FS_Coupon
    | FS_Amount
    | FS_Currency
    | FS_DisplayAmount
    | FS_Expiration
    | FS_DisplayString
    | FS_Type deriving Show

instance A.FromJSON FundingSourceDetails where -- FIXME
  parseJSON (A.Number 1) = return FS_Id
  parseJSON (A.Number 2) = return FS_Coupon
  parseJSON (A.Number 3) = return FS_Amount
  parseJSON (A.Number 7) = return FS_Currency
  parseJSON (A.Number 9) = return FS_DisplayAmount
  parseJSON (A.Number 101) = return FS_Expiration
  parseJSON (A.Number 100) = return FS_DisplayString
  parseJSON (A.Number 102) = return FS_Type
  parseJSON _ = mzero

instance A.ToJSON FundingSourceDetails where
  toJSON FS_Id = A.Number 1
  toJSON FS_Coupon = A.Number 2
  toJSON FS_Amount = A.Number 3
  toJSON FS_Currency = A.Number 7
  toJSON FS_DisplayAmount = A.Number 9
  toJSON FS_Expiration = A.Number 101
  toJSON FS_DisplayString = A.Number 100
  toJSON FS_Type = A.Number 102

data FSD = FSD
instance Field FSD where
    type FieldValue FSD = FundingSourceDetails
    fieldName _ = "funding_source_details"
    fieldLabel = FSD

data AdAccountGroups = AdAccountGroups
instance Field AdAccountGroups where
    type FieldValue AdAccountGroups = V.Vector AAGP.AdAccountGroupResult :*: Nil -- inner ext. record
    fieldName _ = "account_groups"
    fieldLabel = AdAccountGroups

data FAdAccountId = FAdAccountId
instance Field FAdAccountId where
    type FieldValue FAdAccountId = Id
    fieldName _ = "account_id"
    fieldLabel = FAdAccountId

data AdAccountAge = AdAccountAge
instance Field AdAccountAge where
    type FieldValue AdAccountAge = Float
    fieldName _ = "age"
    fieldLabel = AdAccountAge

data AdAccountACD = AdAccountACD
instance Field AdAccountACD where
    type FieldValue AdAccountACD = AgencyClientDeclaration
    fieldName _ = "agency_client_declaration"
    fieldLabel = AdAccountACD

data AgencyClientDeclaration = AgencyClientDeclaration {
    agency_representing_client  :: Int } -- TODO
    deriving Generic
instance A.FromJSON AgencyClientDeclaration
instance A.ToJSON AgencyClientDeclaration

data AdAccountBusCity = AdAccountBusCity
instance Field AdAccountBusCity where
    type FieldValue AdAccountBusCity = Text
    fieldName _ = "business_city"
    fieldLabel = AdAccountBusCity

data AdAccountBusCountry = AdAccountBusCountry
instance Field AdAccountBusCountry where
    type FieldValue AdAccountBusCountry = Text
    fieldName _ = "business_country_code"
    fieldLabel = AdAccountBusCountry

data AdAccountBusName = AdAccountBusName
instance Field AdAccountBusName where
    type FieldValue AdAccountBusName = Text
    fieldName _ = "business_name"
    fieldLabel = AdAccountBusName
-- TODO

testRec = (FId, "TEST_ID") :*: (FSD, FS_Coupon) :*: Nil
getter rec = (rec `get` FId, rec `get` FSD)

getAdAccountId :: (R.MonadResource m, MonadBaseControl IO m) =>
          UserAccessToken -- ^ User access token.
--        -> FacebookT anyAuth m (Pager AdAccountIdDetails)
        -> FacebookT anyAuth m (Pager (FId :*: FAdAccountId :*: Nil))
getAdAccountId token = getObject "/v2.5/me/adaccounts" [] (Just token)

accId rec = rec `get` FAdAccountId
--getAdAccount :: (R.MonadResource m, MonadBaseControl IO m) =>
--           AdAccountId    -- ^ Ad Account Id
--        -> [Argument]     -- ^ Arguments to be passed to Facebook.
--        -> Maybe UserAccessToken -- ^ Optional user access token.
--        -> FacebookT anyAuth m AdAccount
--getAdAccount id_ query mtoken = getObject ("/v2.5/" <> toFbText id_) query mtoken
