{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.AdAccount where

import Facebook.Records hiding (get)
import qualified Facebook.Records as Rec
import Facebook.Types hiding (Id)
import Facebook.Pager
import Facebook.Monad
import Facebook.Graph
import qualified Data.Aeson as A
import Data.Time.Clock
import Data.Time.Format
import Data.Aeson hiding (Value)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Monad.Trans.Resource as R
import Control.Monad.Trans.Control (MonadBaseControl)
import Facebook.Object.Marketing.Types

data SpendCapAction = SpendCapAction
newtype SpendCapAction_ = SpendCapAction_ Text deriving (Show, Generic)
instance A.FromJSON SpendCapAction_
instance A.ToJSON SpendCapAction_
instance Field SpendCapAction where
	type FieldValue SpendCapAction = SpendCapAction_
	fieldName _ = "spend_cap_action"
	fieldLabel = SpendCapAction

data BusinessInfo = BusinessInfo
newtype BusinessInfo_ = BusinessInfo_ A.Value deriving (Show, Generic)
instance A.FromJSON BusinessInfo_
instance A.ToJSON BusinessInfo_
instance Field BusinessInfo where
	type FieldValue BusinessInfo = BusinessInfo_
	fieldName _ = "business_info"
	fieldLabel = BusinessInfo

data FundingId = FundingId
newtype FundingId_ = FundingId_ Text deriving (Show, Generic)
instance A.FromJSON FundingId_
instance A.ToJSON FundingId_
instance Field FundingId where
	type FieldValue FundingId = FundingId_
	fieldName _ = "funding_id"
	fieldLabel = FundingId

data Invoice = Invoice
newtype Invoice_ = Invoice_ Bool deriving (Show, Generic)
instance A.FromJSON Invoice_
instance A.ToJSON Invoice_
instance Field Invoice where
	type FieldValue Invoice = Invoice_
	fieldName _ = "invoice"
	fieldLabel = Invoice

data Replace = Replace
newtype Replace_ = Replace_ Bool deriving (Show, Generic)
instance A.FromJSON Replace_
instance A.ToJSON Replace_
instance Field Replace where
	type FieldValue Replace = Replace_
	fieldName _ = "replace"
	fieldLabel = Replace

data Io = Io
newtype Io_ = Io_ Bool deriving (Show, Generic)
instance A.FromJSON Io_
instance A.ToJSON Io_
instance Field Io where
	type FieldValue Io = Io_
	fieldName _ = "io"
	fieldLabel = Io

data AdaccountId = AdaccountId
newtype AdaccountId_ = AdaccountId_ Text deriving (Show, Generic)
instance A.FromJSON AdaccountId_
instance A.ToJSON AdaccountId_
instance Field AdaccountId where
	type FieldValue AdaccountId = AdaccountId_
	fieldName _ = "adaccount_id"
	fieldLabel = AdaccountId

data PoNumber = PoNumber
newtype PoNumber_ = PoNumber_ Text deriving (Show, Generic)
instance A.FromJSON PoNumber_
instance A.ToJSON PoNumber_
instance Field PoNumber where
	type FieldValue PoNumber = PoNumber_
	fieldName _ = "po_number"
	fieldLabel = PoNumber

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

data BusinessStreet2 = BusinessStreet2
newtype BusinessStreet2_ = BusinessStreet2_ Text deriving (Show, Generic)
instance A.FromJSON BusinessStreet2_
instance A.ToJSON BusinessStreet2_
instance Field BusinessStreet2 where
	type FieldValue BusinessStreet2 = BusinessStreet2_
	fieldName _ = "business_street2"
	fieldLabel = BusinessStreet2

data AccountStatus = AccountStatus
newtype AccountStatus_ = AccountStatus_ Word32 deriving (Show, Generic)
instance A.FromJSON AccountStatus_
instance A.ToJSON AccountStatus_
instance Field AccountStatus where
	type FieldValue AccountStatus = AccountStatus_
	fieldName _ = "account_status"
	fieldLabel = AccountStatus

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

instance ToBS SpendCapAction_ where
	toBS (SpendCapAction_ a) = toBS a

instance ToBS BusinessInfo_ where
	toBS (BusinessInfo_ a) = toBS a

instance ToBS FundingId_ where
	toBS (FundingId_ a) = toBS a

instance ToBS Invoice_ where
	toBS (Invoice_ a) = toBS a

instance ToBS Replace_ where
	toBS (Replace_ a) = toBS a

instance ToBS Io_ where
	toBS (Io_ a) = toBS a

instance ToBS AdaccountId_ where
	toBS (AdaccountId_ a) = toBS a

instance ToBS PoNumber_ where
	toBS (PoNumber_ a) = toBS a

instance ToBS BusinessCity_ where
	toBS (BusinessCity_ a) = toBS a

instance ToBS FundingSource_ where
	toBS (FundingSource_ a) = toBS a

instance ToBS HasMigratedPermissions_ where
	toBS (HasMigratedPermissions_ a) = toBS a

instance ToBS BusinessCountryCode_ where
	toBS (BusinessCountryCode_ a) = toBS a

instance ToBS AmountSpent_ where
	toBS (AmountSpent_ a) = toBS a

instance ToBS IsTaxIdRequired_ where
	toBS (IsTaxIdRequired_ a) = toBS a

instance ToBS BusinessZip_ where
	toBS (BusinessZip_ a) = toBS a

instance ToBS LastUsedTime_ where
	toBS (LastUsedTime_ a) = toBS a

instance ToBS MinDailyBudget_ where
	toBS (MinDailyBudget_ a) = toBS a

instance ToBS BusinessState_ where
	toBS (BusinessState_ a) = toBS a

instance ToBS Age_ where
	toBS (Age_ a) = toBS a

instance ToBS TimezoneOffsetHoursUtc_ where
	toBS (TimezoneOffsetHoursUtc_ a) = toBS a

instance ToBS BusinessStreet2_ where
	toBS (BusinessStreet2_ a) = toBS a

instance ToBS AccountStatus_ where
	toBS (AccountStatus_ a) = toBS a

instance ToBS BusinessStreet_ where
	toBS (BusinessStreet_ a) = toBS a

instance ToBS EndAdvertiserName_ where
	toBS (EndAdvertiserName_ a) = toBS a

instance ToBS BusinessName_ where
	toBS (BusinessName_ a) = toBS a

instance ToBS TaxIdType_ where
	toBS (TaxIdType_ a) = toBS a

instance ToBS Owner_ where
	toBS (Owner_ a) = toBS a

instance ToBS TaxIdStatus_ where
	toBS (TaxIdStatus_ a) = toBS a

instance ToBS IsPrepayAccount_ where
	toBS (IsPrepayAccount_ a) = toBS a

instance ToBS Balance_ where
	toBS (Balance_ a) = toBS a

instance ToBS DisableReason_ where
	toBS (DisableReason_ a) = toBS a

instance ToBS IsPersonal_ where
	toBS (IsPersonal_ a) = toBS a

instance ToBS MinCampaignGroupSpendCap_ where
	toBS (MinCampaignGroupSpendCap_ a) = toBS a

instance ToBS UserRole_ where
	toBS (UserRole_ a) = toBS a

instance ToBS OffsitePixelsTosAccepted_ where
	toBS (OffsitePixelsTosAccepted_ a) = toBS a

instance ToBS IoNumber_ where
	toBS (IoNumber_ a) = toBS a

instance ToBS TimezoneName_ where
	toBS (TimezoneName_ a) = toBS a

instance ToBS AssetScore_ where
	toBS (AssetScore_ a) = toBS a

instance ToBS TaxId_ where
	toBS (TaxId_ a) = toBS a

instance ToBS VerticalName_ where
	toBS (VerticalName_ a) = toBS a

spend_cap_action r = r `Rec.get` SpendCapAction
business_info r = r `Rec.get` BusinessInfo
funding_id r = r `Rec.get` FundingId
invoice r = r `Rec.get` Invoice
replace r = r `Rec.get` Replace
io r = r `Rec.get` Io
adaccount_id r = r `Rec.get` AdaccountId
po_number r = r `Rec.get` PoNumber
business_city r = r `Rec.get` BusinessCity
funding_source r = r `Rec.get` FundingSource
has_migrated_permissions r = r `Rec.get` HasMigratedPermissions
business_country_code r = r `Rec.get` BusinessCountryCode
amount_spent r = r `Rec.get` AmountSpent
is_tax_id_required r = r `Rec.get` IsTaxIdRequired
business_zip r = r `Rec.get` BusinessZip
last_used_time r = r `Rec.get` LastUsedTime
min_daily_budget r = r `Rec.get` MinDailyBudget
business_state r = r `Rec.get` BusinessState
age r = r `Rec.get` Age
timezone_offset_hours_utc r = r `Rec.get` TimezoneOffsetHoursUtc
business_street2 r = r `Rec.get` BusinessStreet2
account_status r = r `Rec.get` AccountStatus
business_street r = r `Rec.get` BusinessStreet
end_advertiser_name r = r `Rec.get` EndAdvertiserName
business_name r = r `Rec.get` BusinessName
tax_id_type r = r `Rec.get` TaxIdType
owner r = r `Rec.get` Owner
tax_id_status r = r `Rec.get` TaxIdStatus
is_prepay_account r = r `Rec.get` IsPrepayAccount
balance r = r `Rec.get` Balance
disable_reason r = r `Rec.get` DisableReason
is_personal r = r `Rec.get` IsPersonal
min_campaign_group_spend_cap r = r `Rec.get` MinCampaignGroupSpendCap
user_role r = r `Rec.get` UserRole
offsite_pixels_tos_accepted r = r `Rec.get` OffsitePixelsTosAccepted
io_number r = r `Rec.get` IoNumber
timezone_name r = r `Rec.get` TimezoneName
asset_score r = r `Rec.get` AssetScore
tax_id r = r `Rec.get` TaxId
vertical_name r = r `Rec.get` VerticalName
-- Entity:AdAccount, mode:Reading
class IsAdAccountGetField r
instance (IsAdAccountGetField h, IsAdAccountGetField t) => IsAdAccountGetField (h :*: t)
instance IsAdAccountGetField Nil
instance IsAdAccountGetField BusinessCity
instance IsAdAccountGetField FundingSource
instance IsAdAccountGetField Id
instance IsAdAccountGetField HasMigratedPermissions
instance IsAdAccountGetField BusinessCountryCode
instance IsAdAccountGetField AmountSpent
instance IsAdAccountGetField IsTaxIdRequired
instance IsAdAccountGetField BusinessZip
instance IsAdAccountGetField Partner
instance IsAdAccountGetField AccountId
instance IsAdAccountGetField LastUsedTime
instance IsAdAccountGetField MinDailyBudget
instance IsAdAccountGetField BusinessState
instance IsAdAccountGetField Age
instance IsAdAccountGetField TimezoneOffsetHoursUtc
instance IsAdAccountGetField CreatedTime
instance IsAdAccountGetField BusinessStreet2
instance IsAdAccountGetField AccountStatus
instance IsAdAccountGetField MediaAgency
instance IsAdAccountGetField BusinessStreet
instance IsAdAccountGetField EndAdvertiserName
instance IsAdAccountGetField BusinessName
instance IsAdAccountGetField TaxIdType
instance IsAdAccountGetField Owner
instance IsAdAccountGetField IsNotificationsEnabled
instance IsAdAccountGetField TaxIdStatus
instance IsAdAccountGetField IsPrepayAccount
instance IsAdAccountGetField Balance
instance IsAdAccountGetField DisableReason
instance IsAdAccountGetField IsPersonal
instance IsAdAccountGetField TimezoneId
instance IsAdAccountGetField Name
instance IsAdAccountGetField SpendCap
instance IsAdAccountGetField MinCampaignGroupSpendCap
instance IsAdAccountGetField UserRole
instance IsAdAccountGetField OffsitePixelsTosAccepted
instance IsAdAccountGetField IoNumber
instance IsAdAccountGetField TimezoneName
instance IsAdAccountGetField AssetScore
instance IsAdAccountGetField TaxId
instance IsAdAccountGetField VerticalName

type AdAccountGet fl r = (A.FromJSON r, IsAdAccountGetField r, FieldListToRec fl r)
type AdAccountGetRet r = Id :*: AccountId :*: r -- Default fields
getAdAccount :: (R.MonadResource m, MonadBaseControl IO m, AdAccountGet fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (AdAccountGetRet r)
getAdAccount (Id_ id) fl mtoken = getObject ("/v2.5/" <> id <> "") [("fields", textListToBS $ fieldNameList $ Id ::: AccountId ::: fl)] mtoken

type AdAccountIdDetails = Id :*: AccountId :*: Nil

getAdAccountId :: (R.MonadResource m, MonadBaseControl IO m) =>
	 Maybe UserAccessToken -- ^ User access token.
	-> FacebookT anyAuth m (Pager AdAccountIdDetails)
getAdAccountId token = getObject "/v2.5/me/adaccounts" [] token

-- Entity:AdAccount, mode:Creating
class IsAdAccountSetField r
instance (IsAdAccountSetField h, IsAdAccountSetField t) => IsAdAccountSetField (h :*: t)
instance IsAdAccountSetField Nil
instance IsAdAccountSetField MediaAgency
instance IsAdAccountSetField FundingId
instance IsAdAccountSetField Invoice
instance IsAdAccountSetField Partner
instance IsAdAccountSetField Name
instance IsAdAccountSetField Replace
instance IsAdAccountSetField Io
instance IsAdAccountSetField Id
instance IsAdAccountSetField AdaccountId
instance IsAdAccountSetField PoNumber
instance IsAdAccountSetField Adaccounts
instance IsAdAccountSetField TimezoneId

type AdAccountSet r = (Has MediaAgency r, Has Partner r, Has Name r, Has AdaccountId r, Has TimezoneId r, A.FromJSON r, IsAdAccountSetField r, ToForm r)
setAdAccount :: (R.MonadResource m, MonadBaseControl IO m, AdAccountSet r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
setAdAccount (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "") (toForm r) mtoken


-- Entity:AdAccount, mode:Updating
class IsAdAccountUpdField r
instance (IsAdAccountUpdField h, IsAdAccountUpdField t) => IsAdAccountUpdField (h :*: t)
instance IsAdAccountUpdField Nil
instance IsAdAccountUpdField SpendCap
instance IsAdAccountUpdField TimezoneId
instance IsAdAccountUpdField IsNotificationsEnabled
instance IsAdAccountUpdField Partner
instance IsAdAccountUpdField Id
instance IsAdAccountUpdField SpendCapAction
instance IsAdAccountUpdField Redownload
instance IsAdAccountUpdField Name
instance IsAdAccountUpdField MediaAgency
instance IsAdAccountUpdField BusinessInfo

type AdAccountUpd r = (Has Id r, A.FromJSON r, IsAdAccountUpdField r, ToForm r)
updAdAccount :: (R.MonadResource m, MonadBaseControl IO m, AdAccountUpd r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
updAdAccount (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "") (toForm r) mtoken


-- Entity:AdAccount, mode:Deleting
class IsAdAccountDelField r
instance (IsAdAccountDelField h, IsAdAccountDelField t) => IsAdAccountDelField (h :*: t)
instance IsAdAccountDelField Nil
instance IsAdAccountDelField AccountId
instance IsAdAccountDelField Id
instance IsAdAccountDelField Adaccounts

type AdAccountDel r = (Has AccountId r, A.FromJSON r, IsAdAccountDelField r, ToForm r)
delAdAccount :: (R.MonadResource m, MonadBaseControl IO m, AdAccountDel r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
delAdAccount (Id_ id) r mtoken = deleteForm ("/v2.5/" <> id <> "") (toForm r) mtoken

