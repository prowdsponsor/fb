{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.AdAccountGen where

import Facebook.Records
import Facebook.Types hiding (Id)
import Facebook.Pager
import Facebook.Monad
import Facebook.Graph
import qualified Data.Aeson as A
import Data.Time.Clock
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Control.Monad.Trans.Resource as R
import Control.Monad.Trans.Control (MonadBaseControl)

data BusinessCity = BusinessCity
newtype BusinessCity_ = BusinessCity_ Text deriving (Show, Generic)
instance A.FromJSON BusinessCity_
instance A.ToJSON BusinessCity_
instance Field BusinessCity where
	type FieldValue BusinessCity = BusinessCity_
	fieldName _ = "business_city"
	fieldLabel = BusinessCity

data FundingSource = FundingSource
newtype FundingSource_ = FundingSource_ Text deriving (Show, Generic)
instance A.FromJSON FundingSource_
instance A.ToJSON FundingSource_
instance Field FundingSource where
	type FieldValue FundingSource = FundingSource_
	fieldName _ = "funding_source"
	fieldLabel = FundingSource

data Id = Id
newtype Id_ = Id_ Text deriving (Show, Generic)
instance A.FromJSON Id_
instance A.ToJSON Id_
instance Field Id where
	type FieldValue Id = Id_
	fieldName _ = "id"
	fieldLabel = Id

data Capabilities = Capabilities
newtype Capabilities_ = Capabilities_ (Vector Text) deriving (Show, Generic)
instance A.FromJSON Capabilities_
instance A.ToJSON Capabilities_
instance Field Capabilities where
	type FieldValue Capabilities = Capabilities_
	fieldName _ = "capabilities"
	fieldLabel = Capabilities

data HasMigratedPermissions = HasMigratedPermissions
newtype HasMigratedPermissions_ = HasMigratedPermissions_ Bool deriving (Show, Generic)
instance A.FromJSON HasMigratedPermissions_
instance A.ToJSON HasMigratedPermissions_
instance Field HasMigratedPermissions where
	type FieldValue HasMigratedPermissions = HasMigratedPermissions_
	fieldName _ = "has_migrated_permissions"
	fieldLabel = HasMigratedPermissions

data BusinessCountryCode = BusinessCountryCode
newtype BusinessCountryCode_ = BusinessCountryCode_ Text deriving (Show, Generic)
instance A.FromJSON BusinessCountryCode_
instance A.ToJSON BusinessCountryCode_
instance Field BusinessCountryCode where
	type FieldValue BusinessCountryCode = BusinessCountryCode_
	fieldName _ = "business_country_code"
	fieldLabel = BusinessCountryCode

data AmountSpent = AmountSpent
newtype AmountSpent_ = AmountSpent_ Text deriving (Show, Generic)
instance A.FromJSON AmountSpent_
instance A.ToJSON AmountSpent_
instance Field AmountSpent where
	type FieldValue AmountSpent = AmountSpent_
	fieldName _ = "amount_spent"
	fieldLabel = AmountSpent

data IsTaxIdRequired = IsTaxIdRequired
newtype IsTaxIdRequired_ = IsTaxIdRequired_ Bool deriving (Show, Generic)
instance A.FromJSON IsTaxIdRequired_
instance A.ToJSON IsTaxIdRequired_
instance Field IsTaxIdRequired where
	type FieldValue IsTaxIdRequired = IsTaxIdRequired_
	fieldName _ = "is_tax_id_required"
	fieldLabel = IsTaxIdRequired

data BusinessZip = BusinessZip
newtype BusinessZip_ = BusinessZip_ Text deriving (Show, Generic)
instance A.FromJSON BusinessZip_
instance A.ToJSON BusinessZip_
instance Field BusinessZip where
	type FieldValue BusinessZip = BusinessZip_
	fieldName _ = "business_zip"
	fieldLabel = BusinessZip

data Partner = Partner
newtype Partner_ = Partner_ Text deriving (Show, Generic)
instance A.FromJSON Partner_
instance A.ToJSON Partner_
instance Field Partner where
	type FieldValue Partner = Partner_
	fieldName _ = "partner"
	fieldLabel = Partner

data AccountId = AccountId
newtype AccountId_ = AccountId_ Text deriving (Show, Generic)
instance A.FromJSON AccountId_
instance A.ToJSON AccountId_
instance Field AccountId where
	type FieldValue AccountId = AccountId_
	fieldName _ = "account_id"
	fieldLabel = AccountId

data LastUsedTime = LastUsedTime
newtype LastUsedTime_ = LastUsedTime_ UTCTime deriving Generic
instance A.FromJSON LastUsedTime_
instance A.ToJSON LastUsedTime_
instance Field LastUsedTime where
	type FieldValue LastUsedTime = LastUsedTime_
	fieldName _ = "last_used_time"
	fieldLabel = LastUsedTime

data MinDailyBudget = MinDailyBudget
newtype MinDailyBudget_ = MinDailyBudget_ Word32 deriving (Show, Generic)
instance A.FromJSON MinDailyBudget_
instance A.ToJSON MinDailyBudget_
instance Field MinDailyBudget where
	type FieldValue MinDailyBudget = MinDailyBudget_
	fieldName _ = "min_daily_budget"
	fieldLabel = MinDailyBudget

data BusinessState = BusinessState
newtype BusinessState_ = BusinessState_ Text deriving (Show, Generic)
instance A.FromJSON BusinessState_
instance A.ToJSON BusinessState_
instance Field BusinessState where
	type FieldValue BusinessState = BusinessState_
	fieldName _ = "business_state"
	fieldLabel = BusinessState

data Age = Age
newtype Age_ = Age_ Float deriving (Show, Generic)
instance A.FromJSON Age_
instance A.ToJSON Age_
instance Field Age where
	type FieldValue Age = Age_
	fieldName _ = "age"
	fieldLabel = Age

data TimezoneOffsetHoursUtc = TimezoneOffsetHoursUtc
newtype TimezoneOffsetHoursUtc_ = TimezoneOffsetHoursUtc_ Float deriving (Show, Generic)
instance A.FromJSON TimezoneOffsetHoursUtc_
instance A.ToJSON TimezoneOffsetHoursUtc_
instance Field TimezoneOffsetHoursUtc where
	type FieldValue TimezoneOffsetHoursUtc = TimezoneOffsetHoursUtc_
	fieldName _ = "timezone_offset_hours_utc"
	fieldLabel = TimezoneOffsetHoursUtc

data CreatedTime = CreatedTime
newtype CreatedTime_ = CreatedTime_ UTCTime deriving Generic
instance A.FromJSON CreatedTime_
instance A.ToJSON CreatedTime_
instance Field CreatedTime where
	type FieldValue CreatedTime = CreatedTime_
	fieldName _ = "created_time"
	fieldLabel = CreatedTime

data BusinessStreet2 = BusinessStreet2
newtype BusinessStreet2_ = BusinessStreet2_ Text deriving (Show, Generic)
instance A.FromJSON BusinessStreet2_
instance A.ToJSON BusinessStreet2_
instance Field BusinessStreet2 where
	type FieldValue BusinessStreet2 = BusinessStreet2_
	fieldName _ = "business_street2"
	fieldLabel = BusinessStreet2

data LineNumbers = LineNumbers
newtype LineNumbers_ = LineNumbers_ (Vector Word32) deriving (Show, Generic)
instance A.FromJSON LineNumbers_
instance A.ToJSON LineNumbers_
instance Field LineNumbers where
	type FieldValue LineNumbers = LineNumbers_
	fieldName _ = "line_numbers"
	fieldLabel = LineNumbers

data EndAdvertiser = EndAdvertiser
newtype EndAdvertiser_ = EndAdvertiser_ Text deriving (Show, Generic)
instance A.FromJSON EndAdvertiser_
instance A.ToJSON EndAdvertiser_
instance Field EndAdvertiser where
	type FieldValue EndAdvertiser = EndAdvertiser_
	fieldName _ = "end_advertiser"
	fieldLabel = EndAdvertiser

data AccountStatus = AccountStatus
newtype AccountStatus_ = AccountStatus_ Word32 deriving (Show, Generic)
instance A.FromJSON AccountStatus_
instance A.ToJSON AccountStatus_
instance Field AccountStatus where
	type FieldValue AccountStatus = AccountStatus_
	fieldName _ = "account_status"
	fieldLabel = AccountStatus

data MediaAgency = MediaAgency
newtype MediaAgency_ = MediaAgency_ Text deriving (Show, Generic)
instance A.FromJSON MediaAgency_
instance A.ToJSON MediaAgency_
instance Field MediaAgency where
	type FieldValue MediaAgency = MediaAgency_
	fieldName _ = "media_agency"
	fieldLabel = MediaAgency

data BusinessStreet = BusinessStreet
newtype BusinessStreet_ = BusinessStreet_ Text deriving (Show, Generic)
instance A.FromJSON BusinessStreet_
instance A.ToJSON BusinessStreet_
instance Field BusinessStreet where
	type FieldValue BusinessStreet = BusinessStreet_
	fieldName _ = "business_street"
	fieldLabel = BusinessStreet

data EndAdvertiserName = EndAdvertiserName
newtype EndAdvertiserName_ = EndAdvertiserName_ Text deriving (Show, Generic)
instance A.FromJSON EndAdvertiserName_
instance A.ToJSON EndAdvertiserName_
instance Field EndAdvertiserName where
	type FieldValue EndAdvertiserName = EndAdvertiserName_
	fieldName _ = "end_advertiser_name"
	fieldLabel = EndAdvertiserName

data BusinessName = BusinessName
newtype BusinessName_ = BusinessName_ Text deriving (Show, Generic)
instance A.FromJSON BusinessName_
instance A.ToJSON BusinessName_
instance Field BusinessName where
	type FieldValue BusinessName = BusinessName_
	fieldName _ = "business_name"
	fieldLabel = BusinessName

data TaxIdType = TaxIdType
newtype TaxIdType_ = TaxIdType_ Text deriving (Show, Generic)
instance A.FromJSON TaxIdType_
instance A.ToJSON TaxIdType_
instance Field TaxIdType where
	type FieldValue TaxIdType = TaxIdType_
	fieldName _ = "tax_id_type"
	fieldLabel = TaxIdType

data Owner = Owner
newtype Owner_ = Owner_ Text deriving (Show, Generic)
instance A.FromJSON Owner_
instance A.ToJSON Owner_
instance Field Owner where
	type FieldValue Owner = Owner_
	fieldName _ = "owner"
	fieldLabel = Owner

data Currency = Currency
newtype Currency_ = Currency_ Text deriving (Show, Generic)
instance A.FromJSON Currency_
instance A.ToJSON Currency_
instance Field Currency where
	type FieldValue Currency = Currency_
	fieldName _ = "currency"
	fieldLabel = Currency

data IsNotificationsEnabled = IsNotificationsEnabled
newtype IsNotificationsEnabled_ = IsNotificationsEnabled_ Bool deriving (Show, Generic)
instance A.FromJSON IsNotificationsEnabled_
instance A.ToJSON IsNotificationsEnabled_
instance Field IsNotificationsEnabled where
	type FieldValue IsNotificationsEnabled = IsNotificationsEnabled_
	fieldName _ = "is_notifications_enabled"
	fieldLabel = IsNotificationsEnabled

data TaxIdStatus = TaxIdStatus
newtype TaxIdStatus_ = TaxIdStatus_ Word32 deriving (Show, Generic)
instance A.FromJSON TaxIdStatus_
instance A.ToJSON TaxIdStatus_
instance Field TaxIdStatus where
	type FieldValue TaxIdStatus = TaxIdStatus_
	fieldName _ = "tax_id_status"
	fieldLabel = TaxIdStatus

data IsPrepayAccount = IsPrepayAccount
newtype IsPrepayAccount_ = IsPrepayAccount_ Bool deriving (Show, Generic)
instance A.FromJSON IsPrepayAccount_
instance A.ToJSON IsPrepayAccount_
instance Field IsPrepayAccount where
	type FieldValue IsPrepayAccount = IsPrepayAccount_
	fieldName _ = "is_prepay_account"
	fieldLabel = IsPrepayAccount

data Balance = Balance
newtype Balance_ = Balance_ Text deriving (Show, Generic)
instance A.FromJSON Balance_
instance A.ToJSON Balance_
instance Field Balance where
	type FieldValue Balance = Balance_
	fieldName _ = "balance"
	fieldLabel = Balance

data TosAccepted = TosAccepted
newtype TosAccepted_ = TosAccepted_ (Map.Map Text Int) deriving (Show, Generic)
instance A.FromJSON TosAccepted_
instance A.ToJSON TosAccepted_
instance Field TosAccepted where
	type FieldValue TosAccepted = TosAccepted_
	fieldName _ = "tos_accepted"
	fieldLabel = TosAccepted

data DisableReason = DisableReason
newtype DisableReason_ = DisableReason_ Word32 deriving (Show, Generic)
instance A.FromJSON DisableReason_
instance A.ToJSON DisableReason_
instance Field DisableReason where
	type FieldValue DisableReason = DisableReason_
	fieldName _ = "disable_reason"
	fieldLabel = DisableReason

data IsPersonal = IsPersonal
newtype IsPersonal_ = IsPersonal_ Word32 deriving (Show, Generic)
instance A.FromJSON IsPersonal_
instance A.ToJSON IsPersonal_
instance Field IsPersonal where
	type FieldValue IsPersonal = IsPersonal_
	fieldName _ = "is_personal"
	fieldLabel = IsPersonal

data TimezoneId = TimezoneId
newtype TimezoneId_ = TimezoneId_ Word32 deriving (Show, Generic)
instance A.FromJSON TimezoneId_
instance A.ToJSON TimezoneId_
instance Field TimezoneId where
	type FieldValue TimezoneId = TimezoneId_
	fieldName _ = "timezone_id"
	fieldLabel = TimezoneId

data Name = Name
newtype Name_ = Name_ Text deriving (Show, Generic)
instance A.FromJSON Name_
instance A.ToJSON Name_
instance Field Name where
	type FieldValue Name = Name_
	fieldName _ = "name"
	fieldLabel = Name

data SpendCap = SpendCap
newtype SpendCap_ = SpendCap_ Text deriving (Show, Generic)
instance A.FromJSON SpendCap_
instance A.ToJSON SpendCap_
instance Field SpendCap where
	type FieldValue SpendCap = SpendCap_
	fieldName _ = "spend_cap"
	fieldLabel = SpendCap

data MinCampaignGroupSpendCap = MinCampaignGroupSpendCap
newtype MinCampaignGroupSpendCap_ = MinCampaignGroupSpendCap_ Text deriving (Show, Generic)
instance A.FromJSON MinCampaignGroupSpendCap_
instance A.ToJSON MinCampaignGroupSpendCap_
instance Field MinCampaignGroupSpendCap where
	type FieldValue MinCampaignGroupSpendCap = MinCampaignGroupSpendCap_
	fieldName _ = "min_campaign_group_spend_cap"
	fieldLabel = MinCampaignGroupSpendCap

data UserRole = UserRole
newtype UserRole_ = UserRole_ Text deriving (Show, Generic)
instance A.FromJSON UserRole_
instance A.ToJSON UserRole_
instance Field UserRole where
	type FieldValue UserRole = UserRole_
	fieldName _ = "user_role"
	fieldLabel = UserRole

data OffsitePixelsTosAccepted = OffsitePixelsTosAccepted
newtype OffsitePixelsTosAccepted_ = OffsitePixelsTosAccepted_ Bool deriving (Show, Generic)
instance A.FromJSON OffsitePixelsTosAccepted_
instance A.ToJSON OffsitePixelsTosAccepted_
instance Field OffsitePixelsTosAccepted where
	type FieldValue OffsitePixelsTosAccepted = OffsitePixelsTosAccepted_
	fieldName _ = "offsite_pixels_tos_accepted"
	fieldLabel = OffsitePixelsTosAccepted

data IoNumber = IoNumber
newtype IoNumber_ = IoNumber_ Text deriving (Show, Generic)
instance A.FromJSON IoNumber_
instance A.ToJSON IoNumber_
instance Field IoNumber where
	type FieldValue IoNumber = IoNumber_
	fieldName _ = "io_number"
	fieldLabel = IoNumber

data TimezoneName = TimezoneName
newtype TimezoneName_ = TimezoneName_ Text deriving (Show, Generic)
instance A.FromJSON TimezoneName_
instance A.ToJSON TimezoneName_
instance Field TimezoneName where
	type FieldValue TimezoneName = TimezoneName_
	fieldName _ = "timezone_name"
	fieldLabel = TimezoneName

data AssetScore = AssetScore
newtype AssetScore_ = AssetScore_ Float deriving (Show, Generic)
instance A.FromJSON AssetScore_
instance A.ToJSON AssetScore_
instance Field AssetScore where
	type FieldValue AssetScore = AssetScore_
	fieldName _ = "asset_score"
	fieldLabel = AssetScore

data TaxId = TaxId
newtype TaxId_ = TaxId_ Text deriving (Show, Generic)
instance A.FromJSON TaxId_
instance A.ToJSON TaxId_
instance Field TaxId where
	type FieldValue TaxId = TaxId_
	fieldName _ = "tax_id"
	fieldLabel = TaxId

data VerticalName = VerticalName
newtype VerticalName_ = VerticalName_ Text deriving (Show, Generic)
instance A.FromJSON VerticalName_
instance A.ToJSON VerticalName_
instance Field VerticalName where
	type FieldValue VerticalName = VerticalName_
	fieldName _ = "vertical_name"
	fieldLabel = VerticalName

class IsAdAccountField r
instance (IsAdAccountField h, IsAdAccountField t) => IsAdAccountField (h :*: t)
instance IsAdAccountField Nil
instance IsAdAccountField BusinessCity
instance IsAdAccountField FundingSource
instance IsAdAccountField Id
instance IsAdAccountField Capabilities
instance IsAdAccountField HasMigratedPermissions
instance IsAdAccountField BusinessCountryCode
instance IsAdAccountField AmountSpent
instance IsAdAccountField IsTaxIdRequired
instance IsAdAccountField BusinessZip
instance IsAdAccountField Partner
instance IsAdAccountField AccountId
instance IsAdAccountField LastUsedTime
instance IsAdAccountField MinDailyBudget
instance IsAdAccountField BusinessState
instance IsAdAccountField Age
instance IsAdAccountField TimezoneOffsetHoursUtc
instance IsAdAccountField CreatedTime
instance IsAdAccountField BusinessStreet2
instance IsAdAccountField LineNumbers
instance IsAdAccountField EndAdvertiser
instance IsAdAccountField AccountStatus
instance IsAdAccountField MediaAgency
instance IsAdAccountField BusinessStreet
instance IsAdAccountField EndAdvertiserName
instance IsAdAccountField BusinessName
instance IsAdAccountField TaxIdType
instance IsAdAccountField Owner
instance IsAdAccountField Currency
instance IsAdAccountField IsNotificationsEnabled
instance IsAdAccountField TaxIdStatus
instance IsAdAccountField IsPrepayAccount
instance IsAdAccountField Balance
instance IsAdAccountField TosAccepted
instance IsAdAccountField DisableReason
instance IsAdAccountField IsPersonal
instance IsAdAccountField TimezoneId
instance IsAdAccountField Name
instance IsAdAccountField SpendCap
instance IsAdAccountField MinCampaignGroupSpendCap
instance IsAdAccountField UserRole
instance IsAdAccountField OffsitePixelsTosAccepted
instance IsAdAccountField IoNumber
instance IsAdAccountField TimezoneName
instance IsAdAccountField AssetScore
instance IsAdAccountField TaxId
instance IsAdAccountField VerticalName

business_city r = r `get` BusinessCity
funding_source r = r `get` FundingSource
id r = r `get` Id
capabilities r = r `get` Capabilities
has_migrated_permissions r = r `get` HasMigratedPermissions
business_country_code r = r `get` BusinessCountryCode
amount_spent r = r `get` AmountSpent
is_tax_id_required r = r `get` IsTaxIdRequired
business_zip r = r `get` BusinessZip
partner r = r `get` Partner
account_id r = r `get` AccountId
last_used_time r = r `get` LastUsedTime
min_daily_budget r = r `get` MinDailyBudget
business_state r = r `get` BusinessState
age r = r `get` Age
timezone_offset_hours_utc r = r `get` TimezoneOffsetHoursUtc
created_time r = r `get` CreatedTime
business_street2 r = r `get` BusinessStreet2
line_numbers r = r `get` LineNumbers
end_advertiser r = r `get` EndAdvertiser
account_status r = r `get` AccountStatus
media_agency r = r `get` MediaAgency
business_street r = r `get` BusinessStreet
end_advertiser_name r = r `get` EndAdvertiserName
business_name r = r `get` BusinessName
tax_id_type r = r `get` TaxIdType
owner r = r `get` Owner
currency r = r `get` Currency
is_notifications_enabled r = r `get` IsNotificationsEnabled
tax_id_status r = r `get` TaxIdStatus
is_prepay_account r = r `get` IsPrepayAccount
balance r = r `get` Balance
tos_accepted r = r `get` TosAccepted
disable_reason r = r `get` DisableReason
is_personal r = r `get` IsPersonal
timezone_id r = r `get` TimezoneId
name r = r `get` Name
spend_cap r = r `get` SpendCap
min_campaign_group_spend_cap r = r `get` MinCampaignGroupSpendCap
user_role r = r `get` UserRole
offsite_pixels_tos_accepted r = r `get` OffsitePixelsTosAccepted
io_number r = r `get` IoNumber
timezone_name r = r `get` TimezoneName
asset_score r = r `get` AssetScore
tax_id r = r `get` TaxId
vertical_name r = r `get` VerticalName
type GetAdAccount fl r = (Has Id r, Has AccountId r, A.FromJSON r, IsAdAccountField r, FieldListToRec fl r)

type AdAccountIdDetails = Id :*: AccountId :*: Nil

getAdAccountId :: (R.MonadResource m, MonadBaseControl IO m) =>
	UserAccessToken -- ^ User access token.
	-> FacebookT anyAuth m (Pager AdAccountIdDetails)
getAdAccountId token = getObject "/v2.5/me/adaccounts" [] (Just token)

getAdAccount :: (R.MonadResource m, MonadBaseControl IO m, GetAdAccount fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m r
getAdAccount (Id_ id) fl mtoken = getObject ("/v2.5/" <> id) [("fields", textListToBS $ fieldNameList fl)] mtoken
