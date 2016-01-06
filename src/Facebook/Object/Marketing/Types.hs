{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.Types where

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
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad
import System.Locale
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Facebook.Object.Marketing.Utility hiding (toBS)
import Text.Read (readMaybe)
newtype Money = Money {getMoneyInPennies :: Int} deriving (Eq, Ord, Show, Read, Typeable, Generic, Num)

instance FromJSON Money where
	parseJSON (String val) =
		case readMaybe $ unpack val of
			Just x -> return $ Money x
			Nothing -> fail $ "Money parser string: "++show (unpack val)
		where
			textToBSL :: Text -> BSL.ByteString
			textToBSL = BSL.fromStrict . TE.encodeUtf8
	parseJSON n@(Number _) = do
		x <- parseJSON n
		return $ Money x
	parseJSON v = fail $ "Money parser value: "++show v

instance ToJSON Money where
	toJSON (Money cents) = String $ pack $ show cents

data FbNumeric = FbStringNumeric Text
		| FbIntegerNumeric Int
	deriving (Show, Read, Generic)
instance FromJSON FbNumeric
instance ToJSON FbNumeric

data FBMObjectCreated = FBMObjectCreated
	{ oc_id :: Text
	, oc_success :: Bool
	} deriving (Show, Generic)
instance FromJSON FBMObjectCreated where
	parseJSON = parseJSONWithPrefix "oc_"

class ToFbText a where
	toFbText :: a -> Text
data ConfigureStatusADT = ACTIVE_ | PAUSED_ | DELETED_ | ARCHIVED_ deriving Generic
instance Show ConfigureStatusADT where
	 show ACTIVE_ = "ACTIVE"
	 show PAUSED_ = "PAUSED"
	 show DELETED_ = "DELETED"
	 show ARCHIVED_ = "ARCHIVED"
instance ToJSON ConfigureStatusADT where
	toJSON ACTIVE_ = String "ACTIVE"
	toJSON PAUSED_ = String "PAUSED"
	toJSON DELETED_ = String "DELETED"
	toJSON ARCHIVED_ = String "ARCHIVED"
instance FromJSON ConfigureStatusADT where
	parseJSON (String "ACTIVE") = pure ACTIVE_
	parseJSON (String "PAUSED") = pure PAUSED_
	parseJSON (String "DELETED") = pure DELETED_
	parseJSON (String "ARCHIVED") = pure ARCHIVED_
instance ToBS ConfigureStatusADT
data EffectiveStatusADT = ACTIVE | PAUSED | DELETED | ARCHIVED | PENDING_REVIEW | DISAPPROVED | PREAPPROVED | PENDING_BILLING_INFO | CAMPAIGN_PAUSED | ADSET_PAUSED deriving (Show, Generic)
instance FromJSON EffectiveStatusADT
instance ToJSON EffectiveStatusADT
instance ToBS EffectiveStatusADT

data ExecOption = VALIDATE_ONLY | INCLUDE_WARNINGS deriving (Show, Generic)
instance ToJSON ExecOption
instance FromJSON ExecOption
instance ToBS ExecOption
data OptGoal = NONE | APP_INSTALLS | CLICKS | ENGAGED_USERS | EXTERNAL | EVENT_RESPONSES | IMPRESSIONS | LEAD_GENERATION | LINK_CLICKS | OFFER_CLAIMS | OFFSITE_CONVERSIONS | PAGE_ENGAGEMENT | PAGE_LIKES | POST_ENGAGEMENT | REACH | SOCIAL_IMPRESSIONS | VIDEO_VIEWS deriving (Show, Generic)
instance FromJSON OptGoal

data BidTypeADT = CPC | CPM | MULTI_PREMIUM | ABSOLUTE_OCPM | CPA deriving (Show, Generic)
instance FromJSON BidTypeADT
instance ToJSON BidTypeADT
instance ToBS BidTypeADT
data CallActionType = OPEN_LINK | LIKE_PAGE | SHOP_NOW | PLAY_GAME | INSTALL_APP | USE_APP |INSTALL_MOBILE_APP | USE_MOBILE_APP | BOOK_TRAVEL | LISTEN_MUSIC | WATCH_VIDEO | LEARN_MORE |SIGN_UP | DOWNLOAD | WATCH_MORE | NO_BUTTON | CALL_NOW | BUY_NOW | GET_OFFER | GET_DIRECTIONS |MESSAGE_PAGE | SUBSCRIBE | DONATE_NOW | GET_QUOTE | CONTACT_US | RECORD_NOW | OPEN_MOVIES deriving (Show, Generic)
instance FromJSON CallActionType
instance ToJSON CallActionType
instance ToBS CallActionType
data RunStatusADT = RS_ACTIVE | RS_DELETED
instance Show RunStatusADT where
	show RS_ACTIVE = "ACTIVE"
	show RS_DELETED = "DELETED"
instance FromJSON RunStatusADT where
	parseJSON (String "ACTIVE") = pure RS_ACTIVE
	parseJSON (String "DELETED") = pure RS_DELETED
instance ToBS RunStatusADT
data ObjectiveADT = OBJ_BRAND_AWARENESS | OBJ_CANVAS_APP_ENGAGEMENT | OBJ_CANVAS_APP_INSTALLS | OBJ_CONVERSIONS | OBJ_EVENT_RESPONSES | OBJ_EXTERNAL | OBJ_LEAD_GENERATION | OBJ_LINK_CLICKS | OBJ_LOCAL_AWARENESS | OBJ_MOBILE_APP_ENGAGEMENT | OBJ_MOBILE_APP_INSTALLS | OBJ_OFFER_CLAIMS | OBJ_PAGE_LIKES | OBJ_POST_ENGAGEMENT | OBJ_PRODUCT_CATALOG_SALES | OBJ_VIDEO_VIEWS
instance Show ObjectiveADT where
	show OBJ_BRAND_AWARENESS = "BRAND_AWARENESS"
	show OBJ_CANVAS_APP_ENGAGEMENT = "CANVAS_APP_ENGAGEMENT"
	show OBJ_CANVAS_APP_INSTALLS = "CANVAS_APP_INSTALLS"
	show OBJ_CONVERSIONS = "CONVERSIONS"
	show OBJ_EVENT_RESPONSES = "EVENT_RESPONSES"
	show OBJ_EXTERNAL = "EXTERNAL"
	show OBJ_LEAD_GENERATION = "LEAD_GENERATION"
	show OBJ_LINK_CLICKS = "LINK_CLICKS"
	show OBJ_LOCAL_AWARENESS = "LOCAL_AWARENESS"
	show OBJ_MOBILE_APP_ENGAGEMENT = "MOBILE_APP_ENGAGEMENT"
	show OBJ_MOBILE_APP_INSTALLS = "MOBILE_APP_INSTALLS"
	show OBJ_OFFER_CLAIMS = "OFFER_CLAIMS"
	show OBJ_PAGE_LIKES = "PAGE_LIKES"
	show OBJ_POST_ENGAGEMENT = "POST_ENGAGEMENT"
	show OBJ_PRODUCT_CATALOG_SALES = "PRODUCT_CATALOG_SALES"
	show OBJ_VIDEO_VIEWS = "VIDEO_VIEWS"
instance ToBS ObjectiveADT
instance ToJSON ObjectiveADT where
	toJSON = toJSON . show
instance FromJSON ObjectiveADT where
	parseJSON (String "BRAND_AWARENESS") = pure OBJ_BRAND_AWARENESS
	parseJSON (String "CANVAS_APP_ENGAGEMENT") = pure OBJ_CANVAS_APP_ENGAGEMENT
	parseJSON (String "CANVAS_APP_INSTALLS") = pure OBJ_CANVAS_APP_INSTALLS
	parseJSON (String "CONVERSIONS") = pure OBJ_CONVERSIONS
	parseJSON (String "EVENT_RESPONSES") = pure OBJ_EVENT_RESPONSES
	parseJSON (String "EXTERNAL") = pure OBJ_EXTERNAL
	parseJSON (String "LEAD_GENERATION") = pure OBJ_LEAD_GENERATION
	parseJSON (String "LINK_CLICKS") = pure OBJ_LINK_CLICKS
	parseJSON (String "LOCAL_AWARENESS") = pure OBJ_LOCAL_AWARENESS
	parseJSON (String "MOBILE_APP_ENGAGEMENT") = pure OBJ_MOBILE_APP_ENGAGEMENT
	parseJSON (String "MOBILE_APP_INSTALLS") = pure OBJ_MOBILE_APP_INSTALLS
	parseJSON (String "OFFER_CLAIMS") = pure OBJ_OFFER_CLAIMS
	parseJSON (String "PAGE_LIKES") = pure OBJ_PAGE_LIKES
	parseJSON (String "POST_ENGAGEMENT") = pure OBJ_POST_ENGAGEMENT
	parseJSON (String "PRODUCT_CATALOG_SALES") = pure OBJ_PRODUCT_CATALOG_SALES
	parseJSON (String "VIDEO_VIEWS") = pure OBJ_VIDEO_VIEWS
data BuyingTypeADT = AUCTION | RESERVED deriving (Show, Generic)
instance FromJSON BuyingTypeADT
instance ToJSON BuyingTypeADT
instance ToBS BuyingTypeADT

data Success = Success {
	success :: Bool
	} deriving (Show, Generic)
instance FromJSON Success

data SuccessId = SuccessId {
	   id_ :: Text
	} deriving (Show, Generic)
instance FromJSON SuccessId where
	parseJSON (Object v) =
			SuccessId <$> v .: "id"

instance ToBS Text where
	toBS = TE.encodeUtf8
instance ToBS Char where
	toBS = B8.singleton
instance ToBS Integer
instance ToBS Bool
instance ToBS A.Value
instance ToBS Word32
instance ToBS Float
instance ToBS a => ToBS (Vector a) where
	toBS xs = V.foldl' BS.append BS.empty $ V.map toBS xs
instance ToBS UTCTime where
	toBS t = B8.pack $ formatTime defaultTimeLocale rfc822DateFormat t

data Id = Id
newtype Id_ = Id_ Text deriving (Show, Generic)
instance A.FromJSON Id_
instance A.ToJSON Id_
instance Field Id where
	type FieldValue Id = Id_
	fieldName _ = "id"
	fieldLabel = Id

data AccountId = AccountId
newtype AccountId_ = AccountId_ Text deriving (Show, Generic)
instance A.FromJSON AccountId_
instance A.ToJSON AccountId_
instance Field AccountId where
	type FieldValue AccountId = AccountId_
	fieldName _ = "account_id"
	fieldLabel = AccountId

data ExecutionOptions = ExecutionOptions
newtype ExecutionOptions_ = ExecutionOptions_ (Vector ExecOption) deriving (Show, Generic)
instance A.FromJSON ExecutionOptions_
instance A.ToJSON ExecutionOptions_
instance Field ExecutionOptions where
	type FieldValue ExecutionOptions = ExecutionOptions_
	fieldName _ = "execution_options"
	fieldLabel = ExecutionOptions

data BidAmount = BidAmount
newtype BidAmount_ = BidAmount_ Integer deriving (Show, Generic)
instance A.FromJSON BidAmount_
instance A.ToJSON BidAmount_
instance Field BidAmount where
	type FieldValue BidAmount = BidAmount_
	fieldName _ = "bid_amount"
	fieldLabel = BidAmount

data DailyImps = DailyImps
newtype DailyImps_ = DailyImps_ Word32 deriving (Show, Generic)
instance A.FromJSON DailyImps_
instance A.ToJSON DailyImps_
instance Field DailyImps where
	type FieldValue DailyImps = DailyImps_
	fieldName _ = "daily_imps"
	fieldLabel = DailyImps

data EndMinute = EndMinute
newtype EndMinute_ = EndMinute_ Word32 deriving (Show, Generic)
instance A.FromJSON EndMinute_
instance A.ToJSON EndMinute_
instance Field EndMinute where
	type FieldValue EndMinute = EndMinute_
	fieldName _ = "end_minute"
	fieldLabel = EndMinute

data ApplicationId = ApplicationId
newtype ApplicationId_ = ApplicationId_ Integer deriving (Show, Generic)
instance A.FromJSON ApplicationId_
instance A.ToJSON ApplicationId_
instance Field ApplicationId where
	type FieldValue ApplicationId = ApplicationId_
	fieldName _ = "application_id"
	fieldLabel = ApplicationId

data IsAutobid = IsAutobid
newtype IsAutobid_ = IsAutobid_ Bool deriving (Show, Generic)
instance A.FromJSON IsAutobid_
instance A.ToJSON IsAutobid_
instance Field IsAutobid where
	type FieldValue IsAutobid = IsAutobid_
	fieldName _ = "is_autobid"
	fieldLabel = IsAutobid

data StartMinute = StartMinute
newtype StartMinute_ = StartMinute_ Word32 deriving (Show, Generic)
instance A.FromJSON StartMinute_
instance A.ToJSON StartMinute_
instance Field StartMinute where
	type FieldValue StartMinute = StartMinute_
	fieldName _ = "start_minute"
	fieldLabel = StartMinute

data Days = Days
newtype Days_ = Days_ (Vector Word32) deriving (Show, Generic)
instance A.FromJSON Days_
instance A.ToJSON Days_
instance Field Days where
	type FieldValue Days = Days_
	fieldName _ = "days"
	fieldLabel = Days

data PixelId = PixelId
newtype PixelId_ = PixelId_ Text deriving (Show, Generic)
instance A.FromJSON PixelId_
instance A.ToJSON PixelId_
instance Field PixelId where
	type FieldValue PixelId = PixelId_
	fieldName _ = "pixel_id"
	fieldLabel = PixelId

data EndTime = EndTime
newtype EndTime_ = EndTime_ UTCTime deriving Generic
instance A.FromJSON EndTime_
instance A.ToJSON EndTime_
instance Field EndTime where
	type FieldValue EndTime = EndTime_
	fieldName _ = "end_time"
	fieldLabel = EndTime

data Name = Name
newtype Name_ = Name_ Text deriving (Show, Generic)
instance A.FromJSON Name_
instance A.ToJSON Name_
instance Field Name where
	type FieldValue Name = Name_
	fieldName _ = "name"
	fieldLabel = Name

data LifetimeImps = LifetimeImps
newtype LifetimeImps_ = LifetimeImps_ Word32 deriving (Show, Generic)
instance A.FromJSON LifetimeImps_
instance A.ToJSON LifetimeImps_
instance Field LifetimeImps where
	type FieldValue LifetimeImps = LifetimeImps_
	fieldName _ = "lifetime_imps"
	fieldLabel = LifetimeImps

data DailyBudget = DailyBudget
newtype DailyBudget_ = DailyBudget_ Text deriving (Show, Generic)
instance A.FromJSON DailyBudget_
instance A.ToJSON DailyBudget_
instance Field DailyBudget where
	type FieldValue DailyBudget = DailyBudget_
	fieldName _ = "daily_budget"
	fieldLabel = DailyBudget

data OfferId = OfferId
newtype OfferId_ = OfferId_ Text deriving (Show, Generic)
instance A.FromJSON OfferId_
instance A.ToJSON OfferId_
instance Field OfferId where
	type FieldValue OfferId = OfferId_
	fieldName _ = "offer_id"
	fieldLabel = OfferId

data LifetimeBudget = LifetimeBudget
newtype LifetimeBudget_ = LifetimeBudget_ Word32 deriving (Show, Generic)
instance A.FromJSON LifetimeBudget_
instance A.ToJSON LifetimeBudget_
instance Field LifetimeBudget where
	type FieldValue LifetimeBudget = LifetimeBudget_
	fieldName _ = "lifetime_budget"
	fieldLabel = LifetimeBudget

data Redownload = Redownload
newtype Redownload_ = Redownload_ Bool deriving (Show, Generic)
instance A.FromJSON Redownload_
instance A.ToJSON Redownload_
instance Field Redownload where
	type FieldValue Redownload = Redownload_
	fieldName _ = "redownload"
	fieldLabel = Redownload

data ObjectStoreUrl = ObjectStoreUrl
newtype ObjectStoreUrl_ = ObjectStoreUrl_ Text deriving (Show, Generic)
instance A.FromJSON ObjectStoreUrl_
instance A.ToJSON ObjectStoreUrl_
instance Field ObjectStoreUrl where
	type FieldValue ObjectStoreUrl = ObjectStoreUrl_
	fieldName _ = "object_store_url"
	fieldLabel = ObjectStoreUrl

data ProductSetId = ProductSetId
newtype ProductSetId_ = ProductSetId_ Text deriving (Show, Generic)
instance A.FromJSON ProductSetId_
instance A.ToJSON ProductSetId_
instance Field ProductSetId where
	type FieldValue ProductSetId = ProductSetId_
	fieldName _ = "product_set_id"
	fieldLabel = ProductSetId

data LifetimeFrequencyCap = LifetimeFrequencyCap
newtype LifetimeFrequencyCap_ = LifetimeFrequencyCap_ Word32 deriving (Show, Generic)
instance A.FromJSON LifetimeFrequencyCap_
instance A.ToJSON LifetimeFrequencyCap_
instance Field LifetimeFrequencyCap where
	type FieldValue LifetimeFrequencyCap = LifetimeFrequencyCap_
	fieldName _ = "lifetime_frequency_cap"
	fieldLabel = LifetimeFrequencyCap

data ProductCatalogId = ProductCatalogId
newtype ProductCatalogId_ = ProductCatalogId_ Text deriving (Show, Generic)
instance A.FromJSON ProductCatalogId_
instance A.ToJSON ProductCatalogId_
instance Field ProductCatalogId where
	type FieldValue ProductCatalogId = ProductCatalogId_
	fieldName _ = "product_catalog_id"
	fieldLabel = ProductCatalogId

data StartTime = StartTime
newtype StartTime_ = StartTime_ UTCTime deriving Generic
instance A.FromJSON StartTime_
instance A.ToJSON StartTime_
instance Field StartTime where
	type FieldValue StartTime = StartTime_
	fieldName _ = "start_time"
	fieldLabel = StartTime

data CreativeSequence = CreativeSequence
newtype CreativeSequence_ = CreativeSequence_ (Vector Text) deriving (Show, Generic)
instance A.FromJSON CreativeSequence_
instance A.ToJSON CreativeSequence_
instance Field CreativeSequence where
	type FieldValue CreativeSequence = CreativeSequence_
	fieldName _ = "creative_sequence"
	fieldLabel = CreativeSequence

data TimeStop = TimeStop
newtype TimeStop_ = TimeStop_ UTCTime deriving Generic
instance A.FromJSON TimeStop_
instance A.ToJSON TimeStop_
instance Field TimeStop where
	type FieldValue TimeStop = TimeStop_
	fieldName _ = "time_stop"
	fieldLabel = TimeStop

data TimeStart = TimeStart
newtype TimeStart_ = TimeStart_ UTCTime deriving Generic
instance A.FromJSON TimeStart_
instance A.ToJSON TimeStart_
instance Field TimeStart where
	type FieldValue TimeStart = TimeStart_
	fieldName _ = "time_start"
	fieldLabel = TimeStart

data PageId = PageId
newtype PageId_ = PageId_ Integer deriving (Show, Generic)
instance A.FromJSON PageId_
instance A.ToJSON PageId_
instance Field PageId where
	type FieldValue PageId = PageId_
	fieldName _ = "page_id"
	fieldLabel = PageId

data CampaignId = CampaignId
newtype CampaignId_ = CampaignId_ Text deriving (Show, Generic)
instance A.FromJSON CampaignId_
instance A.ToJSON CampaignId_
instance Field CampaignId where
	type FieldValue CampaignId = CampaignId_
	fieldName _ = "campaign_id"
	fieldLabel = CampaignId

data RtbFlag = RtbFlag
newtype RtbFlag_ = RtbFlag_ Bool deriving (Show, Generic)
instance A.FromJSON RtbFlag_
instance A.ToJSON RtbFlag_
instance Field RtbFlag where
	type FieldValue RtbFlag = RtbFlag_
	fieldName _ = "rtb_flag"
	fieldLabel = RtbFlag

data ConfiguredStatus = ConfiguredStatus
newtype ConfiguredStatus_ = ConfiguredStatus_ ConfigureStatusADT deriving (Show, Generic)
instance A.FromJSON ConfiguredStatus_
instance A.ToJSON ConfiguredStatus_
instance Field ConfiguredStatus where
	type FieldValue ConfiguredStatus = ConfiguredStatus_
	fieldName _ = "configured_status"
	fieldLabel = ConfiguredStatus

data EffectiveStatus = EffectiveStatus
newtype EffectiveStatus_ = EffectiveStatus_ EffectiveStatusADT deriving (Show, Generic)
instance A.FromJSON EffectiveStatus_
instance A.ToJSON EffectiveStatus_
instance Field EffectiveStatus where
	type FieldValue EffectiveStatus = EffectiveStatus_
	fieldName _ = "effective_status"
	fieldLabel = EffectiveStatus

data CreatedTime = CreatedTime
newtype CreatedTime_ = CreatedTime_ UTCTime deriving Generic
instance A.FromJSON CreatedTime_
instance A.ToJSON CreatedTime_
instance Field CreatedTime where
	type FieldValue CreatedTime = CreatedTime_
	fieldName _ = "created_time"
	fieldLabel = CreatedTime

data UpdatedTime = UpdatedTime
newtype UpdatedTime_ = UpdatedTime_ UTCTime deriving Generic
instance A.FromJSON UpdatedTime_
instance A.ToJSON UpdatedTime_
instance Field UpdatedTime where
	type FieldValue UpdatedTime = UpdatedTime_
	fieldName _ = "updated_time"
	fieldLabel = UpdatedTime

data Hash = Hash
newtype Hash_ = Hash_ Text deriving (Show, Generic)
instance A.FromJSON Hash_
instance A.ToJSON Hash_
instance Field Hash where
	type FieldValue Hash = Hash_
	fieldName _ = "hash"
	fieldLabel = Hash

data RunStatus = RunStatus
newtype RunStatus_ = RunStatus_ Word32 deriving (Show, Generic)
instance A.FromJSON RunStatus_
instance A.ToJSON RunStatus_
instance Field RunStatus where
	type FieldValue RunStatus = RunStatus_
	fieldName _ = "run_status"
	fieldLabel = RunStatus

data ActorImageUrl = ActorImageUrl
newtype ActorImageUrl_ = ActorImageUrl_ Text deriving (Show, Generic)
instance A.FromJSON ActorImageUrl_
instance A.ToJSON ActorImageUrl_
instance Field ActorImageUrl where
	type FieldValue ActorImageUrl = ActorImageUrl_
	fieldName _ = "actor_image_url"
	fieldLabel = ActorImageUrl

data ActorImageHash = ActorImageHash
newtype ActorImageHash_ = ActorImageHash_ Text deriving (Show, Generic)
instance A.FromJSON ActorImageHash_
instance A.ToJSON ActorImageHash_
instance Field ActorImageHash where
	type FieldValue ActorImageHash = ActorImageHash_
	fieldName _ = "actor_image_hash"
	fieldLabel = ActorImageHash

data LinkOgId = LinkOgId
newtype LinkOgId_ = LinkOgId_ Text deriving (Show, Generic)
instance A.FromJSON LinkOgId_
instance A.ToJSON LinkOgId_
instance Field LinkOgId where
	type FieldValue LinkOgId = LinkOgId_
	fieldName _ = "link_og_id"
	fieldLabel = LinkOgId

data ActorName = ActorName
newtype ActorName_ = ActorName_ Text deriving (Show, Generic)
instance A.FromJSON ActorName_
instance A.ToJSON ActorName_
instance Field ActorName where
	type FieldValue ActorName = ActorName_
	fieldName _ = "actor_name"
	fieldLabel = ActorName

data ObjectId = ObjectId
newtype ObjectId_ = ObjectId_ Word32 deriving (Show, Generic)
instance A.FromJSON ObjectId_
instance A.ToJSON ObjectId_
instance Field ObjectId where
	type FieldValue ObjectId = ObjectId_
	fieldName _ = "object_id"
	fieldLabel = ObjectId

data InstagramActorId = InstagramActorId
newtype InstagramActorId_ = InstagramActorId_ Text deriving (Show, Generic)
instance A.FromJSON InstagramActorId_
instance A.ToJSON InstagramActorId_
instance Field InstagramActorId where
	type FieldValue InstagramActorId = InstagramActorId_
	fieldName _ = "instagram_actor_id"
	fieldLabel = InstagramActorId

data ActorId = ActorId
newtype ActorId_ = ActorId_ Word32 deriving (Show, Generic)
instance A.FromJSON ActorId_
instance A.ToJSON ActorId_
instance Field ActorId where
	type FieldValue ActorId = ActorId_
	fieldName _ = "actor_id"
	fieldLabel = ActorId

data ThumbnailUrl = ThumbnailUrl
newtype ThumbnailUrl_ = ThumbnailUrl_ Text deriving (Show, Generic)
instance A.FromJSON ThumbnailUrl_
instance A.ToJSON ThumbnailUrl_
instance Field ThumbnailUrl where
	type FieldValue ThumbnailUrl = ThumbnailUrl_
	fieldName _ = "thumbnail_url"
	fieldLabel = ThumbnailUrl

data TemplateUrl = TemplateUrl
newtype TemplateUrl_ = TemplateUrl_ Text deriving (Show, Generic)
instance A.FromJSON TemplateUrl_
instance A.ToJSON TemplateUrl_
instance Field TemplateUrl where
	type FieldValue TemplateUrl = TemplateUrl_
	fieldName _ = "template_url"
	fieldLabel = TemplateUrl

data LinkUrl = LinkUrl
newtype LinkUrl_ = LinkUrl_ Text deriving (Show, Generic)
instance A.FromJSON LinkUrl_
instance A.ToJSON LinkUrl_
instance Field LinkUrl where
	type FieldValue LinkUrl = LinkUrl_
	fieldName _ = "link_url"
	fieldLabel = LinkUrl

data ObjectStoryId = ObjectStoryId
newtype ObjectStoryId_ = ObjectStoryId_ Text deriving (Show, Generic)
instance A.FromJSON ObjectStoryId_
instance A.ToJSON ObjectStoryId_
instance Field ObjectStoryId where
	type FieldValue ObjectStoryId = ObjectStoryId_
	fieldName _ = "object_story_id"
	fieldLabel = ObjectStoryId

data UrlTags = UrlTags
newtype UrlTags_ = UrlTags_ Text deriving (Show, Generic)
instance A.FromJSON UrlTags_
instance A.ToJSON UrlTags_
instance Field UrlTags where
	type FieldValue UrlTags = UrlTags_
	fieldName _ = "url_tags"
	fieldLabel = UrlTags

data ImageHash = ImageHash
newtype ImageHash_ = ImageHash_ Text deriving (Show, Generic)
instance A.FromJSON ImageHash_
instance A.ToJSON ImageHash_
instance Field ImageHash where
	type FieldValue ImageHash = ImageHash_
	fieldName _ = "image_hash"
	fieldLabel = ImageHash

data Title = Title
newtype Title_ = Title_ Text deriving (Show, Generic)
instance A.FromJSON Title_
instance A.ToJSON Title_
instance Field Title where
	type FieldValue Title = Title_
	fieldName _ = "title"
	fieldLabel = Title

data ObjectUrl = ObjectUrl
newtype ObjectUrl_ = ObjectUrl_ Text deriving (Show, Generic)
instance A.FromJSON ObjectUrl_
instance A.ToJSON ObjectUrl_
instance Field ObjectUrl where
	type FieldValue ObjectUrl = ObjectUrl_
	fieldName _ = "object_url"
	fieldLabel = ObjectUrl

data Body = Body
newtype Body_ = Body_ Text deriving (Show, Generic)
instance A.FromJSON Body_
instance A.ToJSON Body_
instance Field Body where
	type FieldValue Body = Body_
	fieldName _ = "body"
	fieldLabel = Body

data ImageUrl = ImageUrl
newtype ImageUrl_ = ImageUrl_ Text deriving (Show, Generic)
instance A.FromJSON ImageUrl_
instance A.ToJSON ImageUrl_
instance Field ImageUrl where
	type FieldValue ImageUrl = ImageUrl_
	fieldName _ = "image_url"
	fieldLabel = ImageUrl

data SpendCap = SpendCap
newtype SpendCap_ = SpendCap_ Text deriving (Show, Generic)
instance A.FromJSON SpendCap_
instance A.ToJSON SpendCap_
instance Field SpendCap where
	type FieldValue SpendCap = SpendCap_
	fieldName _ = "spend_cap"
	fieldLabel = SpendCap

data Objective = Objective
newtype Objective_ = Objective_ ObjectiveADT deriving (Show, Generic)
instance A.FromJSON Objective_
instance A.ToJSON Objective_
instance Field Objective where
	type FieldValue Objective = Objective_
	fieldName _ = "objective"
	fieldLabel = Objective

data BuyingType = BuyingType
newtype BuyingType_ = BuyingType_ BuyingTypeADT deriving (Show, Generic)
instance A.FromJSON BuyingType_
instance A.ToJSON BuyingType_
instance Field BuyingType where
	type FieldValue BuyingType = BuyingType_
	fieldName _ = "buying_type"
	fieldLabel = BuyingType

data Adaccounts = Adaccounts
newtype Adaccounts_ = Adaccounts_ (Vector Text) deriving (Show, Generic)
instance A.FromJSON Adaccounts_
instance A.ToJSON Adaccounts_
instance Field Adaccounts where
	type FieldValue Adaccounts = Adaccounts_
	fieldName _ = "adaccounts"
	fieldLabel = Adaccounts

data TimezoneId = TimezoneId
newtype TimezoneId_ = TimezoneId_ Word32 deriving (Show, Generic)
instance A.FromJSON TimezoneId_
instance A.ToJSON TimezoneId_
instance Field TimezoneId where
	type FieldValue TimezoneId = TimezoneId_
	fieldName _ = "timezone_id"
	fieldLabel = TimezoneId

data IsNotificationsEnabled = IsNotificationsEnabled
newtype IsNotificationsEnabled_ = IsNotificationsEnabled_ Bool deriving (Show, Generic)
instance A.FromJSON IsNotificationsEnabled_
instance A.ToJSON IsNotificationsEnabled_
instance Field IsNotificationsEnabled where
	type FieldValue IsNotificationsEnabled = IsNotificationsEnabled_
	fieldName _ = "is_notifications_enabled"
	fieldLabel = IsNotificationsEnabled

data Partner = Partner
newtype Partner_ = Partner_ Text deriving (Show, Generic)
instance A.FromJSON Partner_
instance A.ToJSON Partner_
instance Field Partner where
	type FieldValue Partner = Partner_
	fieldName _ = "partner"
	fieldLabel = Partner

data MediaAgency = MediaAgency
newtype MediaAgency_ = MediaAgency_ Text deriving (Show, Generic)
instance A.FromJSON MediaAgency_
instance A.ToJSON MediaAgency_
instance Field MediaAgency where
	type FieldValue MediaAgency = MediaAgency_
	fieldName _ = "media_agency"
	fieldLabel = MediaAgency

data DisplaySequence = DisplaySequence
newtype DisplaySequence_ = DisplaySequence_ Word32 deriving (Show, Generic)
instance A.FromJSON DisplaySequence_
instance A.ToJSON DisplaySequence_
instance Field DisplaySequence where
	type FieldValue DisplaySequence = DisplaySequence_
	fieldName _ = "display_sequence"
	fieldLabel = DisplaySequence

data CampaignGroupId = CampaignGroupId
newtype CampaignGroupId_ = CampaignGroupId_ Word32 deriving (Show, Generic)
instance A.FromJSON CampaignGroupId_
instance A.ToJSON CampaignGroupId_
instance Field CampaignGroupId where
	type FieldValue CampaignGroupId = CampaignGroupId_
	fieldName _ = "campaign_group_id"
	fieldLabel = CampaignGroupId

data Creative = Creative
newtype Creative_ = Creative_ Text deriving (Show, Generic)
instance A.FromJSON Creative_
instance A.ToJSON Creative_
instance Field Creative where
	type FieldValue Creative = Creative_
	fieldName _ = "creative"
	fieldLabel = Creative

data AdsetId = AdsetId
newtype AdsetId_ = AdsetId_ Word32 deriving (Show, Generic)
instance A.FromJSON AdsetId_
instance A.ToJSON AdsetId_
instance Field AdsetId where
	type FieldValue AdsetId = AdsetId_
	fieldName _ = "adset_id"
	fieldLabel = AdsetId

instance ToBS Id_ where
	toBS (Id_ a) = toBS a

instance ToBS AccountId_ where
	toBS (AccountId_ a) = toBS a

instance ToBS ExecutionOptions_ where
	toBS (ExecutionOptions_ a) = toBS a

instance ToBS BidAmount_ where
	toBS (BidAmount_ a) = toBS a

instance ToBS DailyImps_ where
	toBS (DailyImps_ a) = toBS a

instance ToBS EndMinute_ where
	toBS (EndMinute_ a) = toBS a

instance ToBS ApplicationId_ where
	toBS (ApplicationId_ a) = toBS a

instance ToBS IsAutobid_ where
	toBS (IsAutobid_ a) = toBS a

instance ToBS StartMinute_ where
	toBS (StartMinute_ a) = toBS a

instance ToBS Days_ where
	toBS (Days_ a) = toBS a

instance ToBS PixelId_ where
	toBS (PixelId_ a) = toBS a

instance ToBS EndTime_ where
	toBS (EndTime_ a) = toBS a

instance ToBS Name_ where
	toBS (Name_ a) = toBS a

instance ToBS LifetimeImps_ where
	toBS (LifetimeImps_ a) = toBS a

instance ToBS DailyBudget_ where
	toBS (DailyBudget_ a) = toBS a

instance ToBS OfferId_ where
	toBS (OfferId_ a) = toBS a

instance ToBS LifetimeBudget_ where
	toBS (LifetimeBudget_ a) = toBS a

instance ToBS Redownload_ where
	toBS (Redownload_ a) = toBS a

instance ToBS ObjectStoreUrl_ where
	toBS (ObjectStoreUrl_ a) = toBS a

instance ToBS ProductSetId_ where
	toBS (ProductSetId_ a) = toBS a

instance ToBS LifetimeFrequencyCap_ where
	toBS (LifetimeFrequencyCap_ a) = toBS a

instance ToBS ProductCatalogId_ where
	toBS (ProductCatalogId_ a) = toBS a

instance ToBS StartTime_ where
	toBS (StartTime_ a) = toBS a

instance ToBS CreativeSequence_ where
	toBS (CreativeSequence_ a) = toBS a

instance ToBS TimeStop_ where
	toBS (TimeStop_ a) = toBS a

instance ToBS TimeStart_ where
	toBS (TimeStart_ a) = toBS a

instance ToBS PageId_ where
	toBS (PageId_ a) = toBS a

instance ToBS CampaignId_ where
	toBS (CampaignId_ a) = toBS a

instance ToBS RtbFlag_ where
	toBS (RtbFlag_ a) = toBS a

instance ToBS ConfiguredStatus_ where
	toBS (ConfiguredStatus_ a) = toBS a

instance ToBS EffectiveStatus_ where
	toBS (EffectiveStatus_ a) = toBS a

instance ToBS CreatedTime_ where
	toBS (CreatedTime_ a) = toBS a

instance ToBS UpdatedTime_ where
	toBS (UpdatedTime_ a) = toBS a

instance ToBS Hash_ where
	toBS (Hash_ a) = toBS a

instance ToBS RunStatus_ where
	toBS (RunStatus_ a) = toBS a

instance ToBS ActorImageUrl_ where
	toBS (ActorImageUrl_ a) = toBS a

instance ToBS ActorImageHash_ where
	toBS (ActorImageHash_ a) = toBS a

instance ToBS LinkOgId_ where
	toBS (LinkOgId_ a) = toBS a

instance ToBS ActorName_ where
	toBS (ActorName_ a) = toBS a

instance ToBS ObjectId_ where
	toBS (ObjectId_ a) = toBS a

instance ToBS InstagramActorId_ where
	toBS (InstagramActorId_ a) = toBS a

instance ToBS ActorId_ where
	toBS (ActorId_ a) = toBS a

instance ToBS ThumbnailUrl_ where
	toBS (ThumbnailUrl_ a) = toBS a

instance ToBS TemplateUrl_ where
	toBS (TemplateUrl_ a) = toBS a

instance ToBS LinkUrl_ where
	toBS (LinkUrl_ a) = toBS a

instance ToBS ObjectStoryId_ where
	toBS (ObjectStoryId_ a) = toBS a

instance ToBS UrlTags_ where
	toBS (UrlTags_ a) = toBS a

instance ToBS ImageHash_ where
	toBS (ImageHash_ a) = toBS a

instance ToBS Title_ where
	toBS (Title_ a) = toBS a

instance ToBS ObjectUrl_ where
	toBS (ObjectUrl_ a) = toBS a

instance ToBS Body_ where
	toBS (Body_ a) = toBS a

instance ToBS ImageUrl_ where
	toBS (ImageUrl_ a) = toBS a

instance ToBS SpendCap_ where
	toBS (SpendCap_ a) = toBS a

instance ToBS Objective_ where
	toBS (Objective_ a) = toBS a

instance ToBS BuyingType_ where
	toBS (BuyingType_ a) = toBS a

instance ToBS Adaccounts_ where
	toBS (Adaccounts_ a) = toBS a

instance ToBS TimezoneId_ where
	toBS (TimezoneId_ a) = toBS a

instance ToBS IsNotificationsEnabled_ where
	toBS (IsNotificationsEnabled_ a) = toBS a

instance ToBS Partner_ where
	toBS (Partner_ a) = toBS a

instance ToBS MediaAgency_ where
	toBS (MediaAgency_ a) = toBS a

instance ToBS DisplaySequence_ where
	toBS (DisplaySequence_ a) = toBS a

instance ToBS CampaignGroupId_ where
	toBS (CampaignGroupId_ a) = toBS a

instance ToBS Creative_ where
	toBS (Creative_ a) = toBS a

instance ToBS AdsetId_ where
	toBS (AdsetId_ a) = toBS a

id r = r `Rec.get` Id
account_id r = r `Rec.get` AccountId
execution_options r = r `Rec.get` ExecutionOptions
bid_amount r = r `Rec.get` BidAmount
daily_imps r = r `Rec.get` DailyImps
end_minute r = r `Rec.get` EndMinute
application_id r = r `Rec.get` ApplicationId
is_autobid r = r `Rec.get` IsAutobid
start_minute r = r `Rec.get` StartMinute
days r = r `Rec.get` Days
pixel_id r = r `Rec.get` PixelId
end_time r = r `Rec.get` EndTime
name r = r `Rec.get` Name
lifetime_imps r = r `Rec.get` LifetimeImps
daily_budget r = r `Rec.get` DailyBudget
offer_id r = r `Rec.get` OfferId
lifetime_budget r = r `Rec.get` LifetimeBudget
redownload r = r `Rec.get` Redownload
object_store_url r = r `Rec.get` ObjectStoreUrl
product_set_id r = r `Rec.get` ProductSetId
lifetime_frequency_cap r = r `Rec.get` LifetimeFrequencyCap
product_catalog_id r = r `Rec.get` ProductCatalogId
start_time r = r `Rec.get` StartTime
creative_sequence r = r `Rec.get` CreativeSequence
time_stop r = r `Rec.get` TimeStop
time_start r = r `Rec.get` TimeStart
page_id r = r `Rec.get` PageId
campaign_id r = r `Rec.get` CampaignId
rtb_flag r = r `Rec.get` RtbFlag
configured_status r = r `Rec.get` ConfiguredStatus
effective_status r = r `Rec.get` EffectiveStatus
created_time r = r `Rec.get` CreatedTime
updated_time r = r `Rec.get` UpdatedTime
hash r = r `Rec.get` Hash
run_status r = r `Rec.get` RunStatus
actor_image_url r = r `Rec.get` ActorImageUrl
actor_image_hash r = r `Rec.get` ActorImageHash
link_og_id r = r `Rec.get` LinkOgId
actor_name r = r `Rec.get` ActorName
object_id r = r `Rec.get` ObjectId
instagram_actor_id r = r `Rec.get` InstagramActorId
actor_id r = r `Rec.get` ActorId
thumbnail_url r = r `Rec.get` ThumbnailUrl
template_url r = r `Rec.get` TemplateUrl
link_url r = r `Rec.get` LinkUrl
object_story_id r = r `Rec.get` ObjectStoryId
url_tags r = r `Rec.get` UrlTags
image_hash r = r `Rec.get` ImageHash
title r = r `Rec.get` Title
object_url r = r `Rec.get` ObjectUrl
body r = r `Rec.get` Body
image_url r = r `Rec.get` ImageUrl
spend_cap r = r `Rec.get` SpendCap
objective r = r `Rec.get` Objective
buying_type r = r `Rec.get` BuyingType
adaccounts r = r `Rec.get` Adaccounts
timezone_id r = r `Rec.get` TimezoneId
is_notifications_enabled r = r `Rec.get` IsNotificationsEnabled
partner r = r `Rec.get` Partner
media_agency r = r `Rec.get` MediaAgency
display_sequence r = r `Rec.get` DisplaySequence
campaign_group_id r = r `Rec.get` CampaignGroupId
creative r = r `Rec.get` Creative
adset_id r = r `Rec.get` AdsetId
