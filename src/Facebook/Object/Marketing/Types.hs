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
import Facebook.Base (FacebookException(..))
import qualified Data.Aeson as A
import Data.Time.Clock
import Data.Time.Format
import Data.Aeson hiding (Value)
import Control.Applicative
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text.Encoding as TE
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
import System.Locale hiding (defaultTimeLocale, rfc822DateFormat)
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Facebook.Object.Marketing.Utility hiding (toBS)
import Facebook.Object.Marketing.TargetingSpecs
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
instance ToJSON OptGoal
instance ToBS OptGoal

data BidTypeADT = CPC | CPM | MULTI_PREMIUM | ABSOLUTE_OCPM | CPA deriving (Show, Generic)
instance FromJSON BidTypeADT
instance ToJSON BidTypeADT
instance ToBS BidTypeADT
data CallToActionTypeADT = OPEN_LINK | LIKE_PAGE | SHOP_NOW | PLAY_GAME | INSTALL_APP | USE_APP |INSTALL_MOBILE_APP | USE_MOBILE_APP | BOOK_TRAVEL | LISTEN_MUSIC | WATCH_VIDEO | LEARN_MORE |SIGN_UP | DOWNLOAD | WATCH_MORE | NO_BUTTON | CALL_NOW | BUY_NOW | GET_OFFER | GET_DIRECTIONS |MESSAGE_PAGE | SUBSCRIBE | DONATE_NOW | GET_QUOTE | CONTACT_US | RECORD_NOW | OPEN_MOVIES deriving (Show, Generic)
instance FromJSON CallToActionTypeADT
instance ToJSON CallToActionTypeADT
instance ToBS CallToActionTypeADT
data RunStatusADT = RS_ACTIVE | RS_DELETED
instance Show RunStatusADT where
	show RS_ACTIVE = "ACTIVE"
	show RS_DELETED = "DELETED"
instance FromJSON RunStatusADT where
	parseJSON (String "ACTIVE") = pure RS_ACTIVE
	parseJSON (String "DELETED") = pure RS_DELETED
instance ToBS RunStatusADT
data ObjectiveADT = OBJ_BRAND_AWARENESS | OBJ_CANVAS_APP_ENGAGEMENT | OBJ_CANVAS_APP_INSTALLS | OBJ_CONVERSIONS | OBJ_EVENT_RESPONSES | OBJ_EXTERNAL | OBJ_LEAD_GENERATION | OBJ_LINK_CLICKS | OBJ_LOCAL_AWARENESS | OBJ_MOBILE_APP_ENGAGEMENT | OBJ_MOBILE_APP_INSTALLS | OBJ_OFFER_CLAIMS | OBJ_PAGE_LIKES | OBJ_POST_ENGAGEMENT | OBJ_PRODUCT_CATALOG_SALES | OBJ_VIDEO_VIEWS | OBJ_NONE
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
	show OBJ_NONE = "NONE"
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
	parseJSON (String "NONE") = pure OBJ_NONE
	parseJSON (String "VIDEO_VIEWS") = pure OBJ_VIDEO_VIEWS
data BuyingTypeADT = AUCTION | RESERVED deriving (Show, Generic)
instance FromJSON BuyingTypeADT
instance ToJSON BuyingTypeADT
instance ToBS BuyingTypeADT
data DeleteStrategyADT = DELETE_ANY | DELETE_OLDEST | DELETE_ARCHIVED_BEFORE deriving (Show, Generic)
instance FromJSON DeleteStrategyADT
instance ToJSON DeleteStrategyADT
instance ToBS DeleteStrategyADT
data BillingEventADT = APP_INSTALLS_ | CLICKS_ | IMPRESSIONS_ | LINK_CLICKS_ | OFFER_CLAIMS_ | PAGE_LIKES_ | POST_ENGAGEMENT_ | VIDEO_VIEWS_
instance Show BillingEventADT where
	 show APP_INSTALLS_ = "APP_INSTALLS"
	 show CLICKS_ = "CLICKS"
	 show IMPRESSIONS_ = "IMPRESSIONS"
	 show LINK_CLICKS_ = "LINK_CLICKS"
	 show OFFER_CLAIMS_ = "OFFER_CLAIMS"
	 show PAGE_LIKES_ = "PAGE_LIKES"
	 show POST_ENGAGEMENT_ = "POST_ENGAGEMENT"
	 show VIDEO_VIEWS_ = "VIDEO_VIEWS"
instance ToBS BillingEventADT
instance ToJSON BillingEventADT where
	toJSON = toJSON . show
instance FromJSON BillingEventADT where
	parseJSON (String "APP_INSTALLS") = pure APP_INSTALLS_
	parseJSON (String "IMPRESSIONS") = pure IMPRESSIONS_
	parseJSON (String "CLICKS") = pure CLICKS_
	parseJSON (String "LINK_CLICKS") = pure LINK_CLICKS_
	parseJSON (String "OFFER_CLAIMS") = pure OFFER_CLAIMS_
	parseJSON (String "PAGE_LIKES") = pure PAGE_LIKES_
	parseJSON (String "POST_ENGAGEMENT") = pure POST_ENGAGEMENT_
	parseJSON (String "VIDEO_VIEWS") = pure VIDEO_VIEWS_
data ObjectStorySpecADT = ObjectStorySpecADT {
		linkData  :: AdCreativeLinkData,
		storyPageId  :: FBPageId,
		igId  :: Maybe IgId
	} deriving (Show, Generic)
newtype FBPageId = FBPageId Text deriving (Show, Generic)
instance FromJSON FBPageId
newtype IgId = IgId Text deriving (Show, Generic)
instance FromJSON IgId
instance ToJSON ObjectStorySpecADT where
	toJSON (ObjectStorySpecADT ld (FBPageId pi) Nothing) =
	  object [ "link_data" .= ld,
	           "page_id" .= pi] 
	toJSON (ObjectStorySpecADT ld (FBPageId pi) (Just (IgId ig))) =
	  object [ "link_data" .= ld,
	           "page_id" .= pi, 
	           "instagram_actor_id" .= ig] 
instance FromJSON ObjectStorySpecADT where
	parseJSON (Object v) =
	 ObjectStorySpecADT <$> v .: "link_data"
	                    <*> v .: "page_id"
	                    <*> v .:? "instagram_actor_id"
instance ToBS ObjectStorySpecADT where
	toBS a = toBS $ toJSON a
data AdCreativeLinkData = AdCreativeLinkData {
		caption  :: Text,
		imageHash ::  Hash_,
		link, message :: Text,
		description  :: Maybe Text,
		call_to_action :: Maybe CallToActionADT}
	| CreativeCarouselData {
		caption_carousel, message_carousel :: Text,
		child_attachments ::  CarouselChildren,
		link :: Text }
	deriving (Show, Generic)
instance ToJSON AdCreativeLinkData where
	toJSON (AdCreativeLinkData c i l m (Just d) (Just cta)) =
	  object [ "caption" .= c,
	           "image_hash" .= i,
	           "link" .= l,
	           "message" .= m]
	           --"description" .= d,
	           --"call_to_action" .= cta]
	toJSON (CreativeCarouselData c m cs l) =
	  object [ "caption" .= c,
	           "message" .= m,
	           "child_attachments" .= toJSON cs,
	           "link" .= l]
instance FromJSON AdCreativeLinkData where
	parseJSON (Object v) = do
	 typ <- v .:? "child_attachments" :: Parser (Maybe CarouselChildren)
	 case typ of
		 Nothing -> parseAdCreativeLinkData v
		 Just _  -> parseCreativeCarouselData v

parseAdCreativeLinkData v =
	 AdCreativeLinkData <$> v .: "caption"
	                    <*> v .: "image_hash"
	                    <*> v .: "link"
	                    <*> v .: "message"
	                    <*> v .:? "description"
	                    <*> v .:? "call_to_action"

parseCreativeCarouselData v =
	 CreativeCarouselData <$> v .: "caption"
	                      <*> v .: "message"
	                      <*> v .: "child_attachments"
	                      <*> v .: "link"
instance ToBS AdCreativeLinkData where
	toBS a = toBS $ toJSON a
type CarouselChildren = [CarouselChild]
data CarouselChild = CarouselChild {
		name_car_child :: Text,
		imageHash_car_child ::  Hash_,
		link_car_child :: Text,
	description_car_child  :: Maybe Text}
	deriving (Show, Generic)
instance ToJSON CarouselChild where
	toJSON (CarouselChild n i l (Just d)) =
	  object [ "name" .= n,
	           "image_hash" .= i,
	           "link" .= l,
	           "description" .= d]
instance FromJSON CarouselChild where
	parseJSON (Object v) =
	 CarouselChild <$> v .: "name"
	               <*> v .: "image_hash"
	               <*> v .: "link"
	               <*> v .:? "description"
instance ToBS CarouselChild where
	toBS a = toBS $ toJSON a
data AdCreativeADT = AdCreativeADT {
	creative_id  :: Text
	} deriving (Show, Generic)
instance FromJSON AdCreativeADT
instance ToJSON AdCreativeADT
instance ToBS AdCreativeADT where
	toBS = toBS . toJSON
data CallToActionValue = CallToActionValue {
	ctav_link, ctav_link_caption :: Text
	} deriving (Show, Generic)
instance ToJSON CallToActionValue where
	toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop $ length ("ctav_" :: String)}
instance FromJSON CallToActionValue where
	parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop $ length ("ctav_" :: String)}
data CallToActionADT = CallToActionADT {
		cta_type :: CallToActionTypeADT,
		cta_value :: CallToActionValue
	} deriving (Show, Generic)
instance ToJSON CallToActionADT where
	toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop $ length ("cta_" :: String)}
instance FromJSON CallToActionADT where
	parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop $ length ("cta_" :: String)}

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
instance ToBS Int
instance ToBS Bool where
	toBS True = toBS ("true" :: String)
	toBS False = toBS ("false" :: String)
--instance ToBS Value where
--	toBS = BSL.toStrict . encode
instance ToBS Float
instance ToBS a => ToBS (Vector a) where
	toBS xs = V.foldl' BS.append BS.empty $ V.map toBS xs
instance ToBS UTCTime where
	toBS t = B8.pack $ formatTime defaultTimeLocale rfc822DateFormat t

data Id = Id
newtype Id_ = Id_ Text deriving (Show, Generic)
instance Field Id where
	type FieldValue Id = Id_
	fieldName _ = "id"
	fieldLabel = Id
unId_ :: Id_ -> Text
unId_ (Id_ x) = x

data AccountId = AccountId
newtype AccountId_ = AccountId_ Text deriving (Show, Generic)
instance Field AccountId where
	type FieldValue AccountId = AccountId_
	fieldName _ = "account_id"
	fieldLabel = AccountId
unAccountId_ :: AccountId_ -> Text
unAccountId_ (AccountId_ x) = x

data ExecutionOptions = ExecutionOptions
newtype ExecutionOptions_ = ExecutionOptions_ (Vector ExecOption) deriving (Show, Generic)
instance Field ExecutionOptions where
	type FieldValue ExecutionOptions = ExecutionOptions_
	fieldName _ = "execution_options"
	fieldLabel = ExecutionOptions
unExecutionOptions_ :: ExecutionOptions_ -> Vector ExecOption
unExecutionOptions_ (ExecutionOptions_ x) = x

data OptimizationGoal = OptimizationGoal
newtype OptimizationGoal_ = OptimizationGoal_ OptGoal deriving (Show, Generic)
instance Field OptimizationGoal where
	type FieldValue OptimizationGoal = OptimizationGoal_
	fieldName _ = "optimization_goal"
	fieldLabel = OptimizationGoal
unOptimizationGoal_ :: OptimizationGoal_ -> OptGoal
unOptimizationGoal_ (OptimizationGoal_ x) = x

data BidAmount = BidAmount
newtype BidAmount_ = BidAmount_ Integer deriving (Show, Generic)
instance Field BidAmount where
	type FieldValue BidAmount = BidAmount_
	fieldName _ = "bid_amount"
	fieldLabel = BidAmount
unBidAmount_ :: BidAmount_ -> Integer
unBidAmount_ (BidAmount_ x) = x

data DailyImps = DailyImps
newtype DailyImps_ = DailyImps_ Int deriving (Show, Generic)
instance Field DailyImps where
	type FieldValue DailyImps = DailyImps_
	fieldName _ = "daily_imps"
	fieldLabel = DailyImps
unDailyImps_ :: DailyImps_ -> Int
unDailyImps_ (DailyImps_ x) = x

data EndMinute = EndMinute
newtype EndMinute_ = EndMinute_ Int deriving (Show, Generic)
instance Field EndMinute where
	type FieldValue EndMinute = EndMinute_
	fieldName _ = "end_minute"
	fieldLabel = EndMinute
unEndMinute_ :: EndMinute_ -> Int
unEndMinute_ (EndMinute_ x) = x

data ApplicationId = ApplicationId
newtype ApplicationId_ = ApplicationId_ Integer deriving (Show, Generic)
instance Field ApplicationId where
	type FieldValue ApplicationId = ApplicationId_
	fieldName _ = "application_id"
	fieldLabel = ApplicationId
unApplicationId_ :: ApplicationId_ -> Integer
unApplicationId_ (ApplicationId_ x) = x

data BillingEvent = BillingEvent
newtype BillingEvent_ = BillingEvent_ BillingEventADT deriving (Show, Generic)
instance Field BillingEvent where
	type FieldValue BillingEvent = BillingEvent_
	fieldName _ = "billing_event"
	fieldLabel = BillingEvent
unBillingEvent_ :: BillingEvent_ -> BillingEventADT
unBillingEvent_ (BillingEvent_ x) = x

data IsAutobid = IsAutobid
newtype IsAutobid_ = IsAutobid_ Bool deriving (Show, Generic)
instance Field IsAutobid where
	type FieldValue IsAutobid = IsAutobid_
	fieldName _ = "is_autobid"
	fieldLabel = IsAutobid
unIsAutobid_ :: IsAutobid_ -> Bool
unIsAutobid_ (IsAutobid_ x) = x

data StartMinute = StartMinute
newtype StartMinute_ = StartMinute_ Int deriving (Show, Generic)
instance Field StartMinute where
	type FieldValue StartMinute = StartMinute_
	fieldName _ = "start_minute"
	fieldLabel = StartMinute
unStartMinute_ :: StartMinute_ -> Int
unStartMinute_ (StartMinute_ x) = x

data Days = Days
newtype Days_ = Days_ (Vector Int) deriving (Show, Generic)
instance Field Days where
	type FieldValue Days = Days_
	fieldName _ = "days"
	fieldLabel = Days
unDays_ :: Days_ -> Vector Int
unDays_ (Days_ x) = x

data PixelId = PixelId
newtype PixelId_ = PixelId_ Text deriving (Show, Generic)
instance Field PixelId where
	type FieldValue PixelId = PixelId_
	fieldName _ = "pixel_id"
	fieldLabel = PixelId
unPixelId_ :: PixelId_ -> Text
unPixelId_ (PixelId_ x) = x

data EndTime = EndTime
newtype EndTime_ = EndTime_ UTCTime deriving Generic
instance Field EndTime where
	type FieldValue EndTime = EndTime_
	fieldName _ = "end_time"
	fieldLabel = EndTime
unEndTime_ :: EndTime_ -> UTCTime
unEndTime_ (EndTime_ x) = x

data Name = Name
newtype Name_ = Name_ Text deriving (Show, Generic)
instance Field Name where
	type FieldValue Name = Name_
	fieldName _ = "name"
	fieldLabel = Name
unName_ :: Name_ -> Text
unName_ (Name_ x) = x

data LifetimeImps = LifetimeImps
newtype LifetimeImps_ = LifetimeImps_ Int deriving (Show, Generic)
instance Field LifetimeImps where
	type FieldValue LifetimeImps = LifetimeImps_
	fieldName _ = "lifetime_imps"
	fieldLabel = LifetimeImps
unLifetimeImps_ :: LifetimeImps_ -> Int
unLifetimeImps_ (LifetimeImps_ x) = x

data OfferId = OfferId
newtype OfferId_ = OfferId_ Text deriving (Show, Generic)
instance Field OfferId where
	type FieldValue OfferId = OfferId_
	fieldName _ = "offer_id"
	fieldLabel = OfferId
unOfferId_ :: OfferId_ -> Text
unOfferId_ (OfferId_ x) = x

data LifetimeBudget = LifetimeBudget
newtype LifetimeBudget_ = LifetimeBudget_ Int deriving (Show, Generic)
instance Field LifetimeBudget where
	type FieldValue LifetimeBudget = LifetimeBudget_
	fieldName _ = "lifetime_budget"
	fieldLabel = LifetimeBudget
unLifetimeBudget_ :: LifetimeBudget_ -> Int
unLifetimeBudget_ (LifetimeBudget_ x) = x

data Redownload = Redownload
newtype Redownload_ = Redownload_ Bool deriving (Show, Generic)
instance Field Redownload where
	type FieldValue Redownload = Redownload_
	fieldName _ = "redownload"
	fieldLabel = Redownload
unRedownload_ :: Redownload_ -> Bool
unRedownload_ (Redownload_ x) = x

data ObjectStoreUrl = ObjectStoreUrl
newtype ObjectStoreUrl_ = ObjectStoreUrl_ Text deriving (Show, Generic)
instance Field ObjectStoreUrl where
	type FieldValue ObjectStoreUrl = ObjectStoreUrl_
	fieldName _ = "object_store_url"
	fieldLabel = ObjectStoreUrl
unObjectStoreUrl_ :: ObjectStoreUrl_ -> Text
unObjectStoreUrl_ (ObjectStoreUrl_ x) = x

data ProductSetId = ProductSetId
newtype ProductSetId_ = ProductSetId_ Text deriving (Show, Generic)
instance Field ProductSetId where
	type FieldValue ProductSetId = ProductSetId_
	fieldName _ = "product_set_id"
	fieldLabel = ProductSetId
unProductSetId_ :: ProductSetId_ -> Text
unProductSetId_ (ProductSetId_ x) = x

data LifetimeFrequencyCap = LifetimeFrequencyCap
newtype LifetimeFrequencyCap_ = LifetimeFrequencyCap_ Int deriving (Show, Generic)
instance Field LifetimeFrequencyCap where
	type FieldValue LifetimeFrequencyCap = LifetimeFrequencyCap_
	fieldName _ = "lifetime_frequency_cap"
	fieldLabel = LifetimeFrequencyCap
unLifetimeFrequencyCap_ :: LifetimeFrequencyCap_ -> Int
unLifetimeFrequencyCap_ (LifetimeFrequencyCap_ x) = x

data ProductCatalogId = ProductCatalogId
newtype ProductCatalogId_ = ProductCatalogId_ Text deriving (Show, Generic)
instance Field ProductCatalogId where
	type FieldValue ProductCatalogId = ProductCatalogId_
	fieldName _ = "product_catalog_id"
	fieldLabel = ProductCatalogId
unProductCatalogId_ :: ProductCatalogId_ -> Text
unProductCatalogId_ (ProductCatalogId_ x) = x

data StartTime = StartTime
newtype StartTime_ = StartTime_ UTCTime deriving Generic
instance Field StartTime where
	type FieldValue StartTime = StartTime_
	fieldName _ = "start_time"
	fieldLabel = StartTime
unStartTime_ :: StartTime_ -> UTCTime
unStartTime_ (StartTime_ x) = x

data CreativeSequence = CreativeSequence
newtype CreativeSequence_ = CreativeSequence_ (Vector Text) deriving (Show, Generic)
instance Field CreativeSequence where
	type FieldValue CreativeSequence = CreativeSequence_
	fieldName _ = "creative_sequence"
	fieldLabel = CreativeSequence
unCreativeSequence_ :: CreativeSequence_ -> Vector Text
unCreativeSequence_ (CreativeSequence_ x) = x

data Targeting = Targeting
newtype Targeting_ = Targeting_ TargetingSpecs deriving (Show, Generic)
instance Field Targeting where
	type FieldValue Targeting = Targeting_
	fieldName _ = "targeting"
	fieldLabel = Targeting
unTargeting_ :: Targeting_ -> TargetingSpecs
unTargeting_ (Targeting_ x) = x

data TimeStop = TimeStop
newtype TimeStop_ = TimeStop_ UTCTime deriving Generic
instance Field TimeStop where
	type FieldValue TimeStop = TimeStop_
	fieldName _ = "time_stop"
	fieldLabel = TimeStop
unTimeStop_ :: TimeStop_ -> UTCTime
unTimeStop_ (TimeStop_ x) = x

data TimeStart = TimeStart
newtype TimeStart_ = TimeStart_ UTCTime deriving Generic
instance Field TimeStart where
	type FieldValue TimeStart = TimeStart_
	fieldName _ = "time_start"
	fieldLabel = TimeStart
unTimeStart_ :: TimeStart_ -> UTCTime
unTimeStart_ (TimeStart_ x) = x

data PageId = PageId
newtype PageId_ = PageId_ Integer deriving (Show, Generic)
instance Field PageId where
	type FieldValue PageId = PageId_
	fieldName _ = "page_id"
	fieldLabel = PageId
unPageId_ :: PageId_ -> Integer
unPageId_ (PageId_ x) = x

data CampaignId = CampaignId
newtype CampaignId_ = CampaignId_ Text deriving (Show, Generic)
instance Field CampaignId where
	type FieldValue CampaignId = CampaignId_
	fieldName _ = "campaign_id"
	fieldLabel = CampaignId
unCampaignId_ :: CampaignId_ -> Text
unCampaignId_ (CampaignId_ x) = x

data RtbFlag = RtbFlag
newtype RtbFlag_ = RtbFlag_ Bool deriving (Show, Generic)
instance Field RtbFlag where
	type FieldValue RtbFlag = RtbFlag_
	fieldName _ = "rtb_flag"
	fieldLabel = RtbFlag
unRtbFlag_ :: RtbFlag_ -> Bool
unRtbFlag_ (RtbFlag_ x) = x

data ConfiguredStatus = ConfiguredStatus
newtype ConfiguredStatus_ = ConfiguredStatus_ ConfigureStatusADT deriving (Show, Generic)
instance Field ConfiguredStatus where
	type FieldValue ConfiguredStatus = ConfiguredStatus_
	fieldName _ = "configured_status"
	fieldLabel = ConfiguredStatus
unConfiguredStatus_ :: ConfiguredStatus_ -> ConfigureStatusADT
unConfiguredStatus_ (ConfiguredStatus_ x) = x

data EffectiveStatus = EffectiveStatus
newtype EffectiveStatus_ = EffectiveStatus_ EffectiveStatusADT deriving (Show, Generic)
instance Field EffectiveStatus where
	type FieldValue EffectiveStatus = EffectiveStatus_
	fieldName _ = "effective_status"
	fieldLabel = EffectiveStatus
unEffectiveStatus_ :: EffectiveStatus_ -> EffectiveStatusADT
unEffectiveStatus_ (EffectiveStatus_ x) = x

data CreatedTime = CreatedTime
newtype CreatedTime_ = CreatedTime_ UTCTime deriving Generic
instance Field CreatedTime where
	type FieldValue CreatedTime = CreatedTime_
	fieldName _ = "created_time"
	fieldLabel = CreatedTime
unCreatedTime_ :: CreatedTime_ -> UTCTime
unCreatedTime_ (CreatedTime_ x) = x

data UpdatedTime = UpdatedTime
newtype UpdatedTime_ = UpdatedTime_ UTCTime deriving Generic
instance Field UpdatedTime where
	type FieldValue UpdatedTime = UpdatedTime_
	fieldName _ = "updated_time"
	fieldLabel = UpdatedTime
unUpdatedTime_ :: UpdatedTime_ -> UTCTime
unUpdatedTime_ (UpdatedTime_ x) = x

data Hash = Hash
newtype Hash_ = Hash_ Text deriving (Show, Generic)
instance Field Hash where
	type FieldValue Hash = Hash_
	fieldName _ = "hash"
	fieldLabel = Hash
unHash_ :: Hash_ -> Text
unHash_ (Hash_ x) = x

data RunStatus = RunStatus
newtype RunStatus_ = RunStatus_ Int deriving (Show, Generic)
instance Field RunStatus where
	type FieldValue RunStatus = RunStatus_
	fieldName _ = "run_status"
	fieldLabel = RunStatus
unRunStatus_ :: RunStatus_ -> Int
unRunStatus_ (RunStatus_ x) = x

data ActorImageUrl = ActorImageUrl
newtype ActorImageUrl_ = ActorImageUrl_ Text deriving (Show, Generic)
instance Field ActorImageUrl where
	type FieldValue ActorImageUrl = ActorImageUrl_
	fieldName _ = "actor_image_url"
	fieldLabel = ActorImageUrl
unActorImageUrl_ :: ActorImageUrl_ -> Text
unActorImageUrl_ (ActorImageUrl_ x) = x

data ActorImageHash = ActorImageHash
newtype ActorImageHash_ = ActorImageHash_ Text deriving (Show, Generic)
instance Field ActorImageHash where
	type FieldValue ActorImageHash = ActorImageHash_
	fieldName _ = "actor_image_hash"
	fieldLabel = ActorImageHash
unActorImageHash_ :: ActorImageHash_ -> Text
unActorImageHash_ (ActorImageHash_ x) = x

data LinkOgId = LinkOgId
newtype LinkOgId_ = LinkOgId_ Text deriving (Show, Generic)
instance Field LinkOgId where
	type FieldValue LinkOgId = LinkOgId_
	fieldName _ = "link_og_id"
	fieldLabel = LinkOgId
unLinkOgId_ :: LinkOgId_ -> Text
unLinkOgId_ (LinkOgId_ x) = x

data ActorName = ActorName
newtype ActorName_ = ActorName_ Text deriving (Show, Generic)
instance Field ActorName where
	type FieldValue ActorName = ActorName_
	fieldName _ = "actor_name"
	fieldLabel = ActorName
unActorName_ :: ActorName_ -> Text
unActorName_ (ActorName_ x) = x

data ObjectId = ObjectId
newtype ObjectId_ = ObjectId_ Int deriving (Show, Generic)
instance Field ObjectId where
	type FieldValue ObjectId = ObjectId_
	fieldName _ = "object_id"
	fieldLabel = ObjectId
unObjectId_ :: ObjectId_ -> Int
unObjectId_ (ObjectId_ x) = x

data InstagramActorId = InstagramActorId
newtype InstagramActorId_ = InstagramActorId_ Text deriving (Show, Generic)
instance Field InstagramActorId where
	type FieldValue InstagramActorId = InstagramActorId_
	fieldName _ = "instagram_actor_id"
	fieldLabel = InstagramActorId
unInstagramActorId_ :: InstagramActorId_ -> Text
unInstagramActorId_ (InstagramActorId_ x) = x

data ActorId = ActorId
newtype ActorId_ = ActorId_ Int deriving (Show, Generic)
instance Field ActorId where
	type FieldValue ActorId = ActorId_
	fieldName _ = "actor_id"
	fieldLabel = ActorId
unActorId_ :: ActorId_ -> Int
unActorId_ (ActorId_ x) = x

data ThumbnailUrl = ThumbnailUrl
newtype ThumbnailUrl_ = ThumbnailUrl_ Text deriving (Show, Generic)
instance Field ThumbnailUrl where
	type FieldValue ThumbnailUrl = ThumbnailUrl_
	fieldName _ = "thumbnail_url"
	fieldLabel = ThumbnailUrl
unThumbnailUrl_ :: ThumbnailUrl_ -> Text
unThumbnailUrl_ (ThumbnailUrl_ x) = x

data TemplateUrl = TemplateUrl
newtype TemplateUrl_ = TemplateUrl_ Text deriving (Show, Generic)
instance Field TemplateUrl where
	type FieldValue TemplateUrl = TemplateUrl_
	fieldName _ = "template_url"
	fieldLabel = TemplateUrl
unTemplateUrl_ :: TemplateUrl_ -> Text
unTemplateUrl_ (TemplateUrl_ x) = x

data LinkUrl = LinkUrl
newtype LinkUrl_ = LinkUrl_ Text deriving (Show, Generic)
instance Field LinkUrl where
	type FieldValue LinkUrl = LinkUrl_
	fieldName _ = "link_url"
	fieldLabel = LinkUrl
unLinkUrl_ :: LinkUrl_ -> Text
unLinkUrl_ (LinkUrl_ x) = x

data ObjectStorySpec = ObjectStorySpec
newtype ObjectStorySpec_ = ObjectStorySpec_ ObjectStorySpecADT deriving (Show, Generic)
instance Field ObjectStorySpec where
	type FieldValue ObjectStorySpec = ObjectStorySpec_
	fieldName _ = "object_story_spec"
	fieldLabel = ObjectStorySpec
unObjectStorySpec_ :: ObjectStorySpec_ -> ObjectStorySpecADT
unObjectStorySpec_ (ObjectStorySpec_ x) = x

data ObjectStoryId = ObjectStoryId
newtype ObjectStoryId_ = ObjectStoryId_ Text deriving (Show, Generic)
instance Field ObjectStoryId where
	type FieldValue ObjectStoryId = ObjectStoryId_
	fieldName _ = "object_story_id"
	fieldLabel = ObjectStoryId
unObjectStoryId_ :: ObjectStoryId_ -> Text
unObjectStoryId_ (ObjectStoryId_ x) = x

data UrlTags = UrlTags
newtype UrlTags_ = UrlTags_ Text deriving (Show, Generic)
instance Field UrlTags where
	type FieldValue UrlTags = UrlTags_
	fieldName _ = "url_tags"
	fieldLabel = UrlTags
unUrlTags_ :: UrlTags_ -> Text
unUrlTags_ (UrlTags_ x) = x

data ImageHash = ImageHash
newtype ImageHash_ = ImageHash_ Text deriving (Show, Generic)
instance Field ImageHash where
	type FieldValue ImageHash = ImageHash_
	fieldName _ = "image_hash"
	fieldLabel = ImageHash
unImageHash_ :: ImageHash_ -> Text
unImageHash_ (ImageHash_ x) = x

data Title = Title
newtype Title_ = Title_ Text deriving (Show, Generic)
instance Field Title where
	type FieldValue Title = Title_
	fieldName _ = "title"
	fieldLabel = Title
unTitle_ :: Title_ -> Text
unTitle_ (Title_ x) = x

data ObjectUrl = ObjectUrl
newtype ObjectUrl_ = ObjectUrl_ Text deriving (Show, Generic)
instance Field ObjectUrl where
	type FieldValue ObjectUrl = ObjectUrl_
	fieldName _ = "object_url"
	fieldLabel = ObjectUrl
unObjectUrl_ :: ObjectUrl_ -> Text
unObjectUrl_ (ObjectUrl_ x) = x

data Body = Body
newtype Body_ = Body_ Text deriving (Show, Generic)
instance Field Body where
	type FieldValue Body = Body_
	fieldName _ = "body"
	fieldLabel = Body
unBody_ :: Body_ -> Text
unBody_ (Body_ x) = x

data ImageUrl = ImageUrl
newtype ImageUrl_ = ImageUrl_ Text deriving (Show, Generic)
instance Field ImageUrl where
	type FieldValue ImageUrl = ImageUrl_
	fieldName _ = "image_url"
	fieldLabel = ImageUrl
unImageUrl_ :: ImageUrl_ -> Text
unImageUrl_ (ImageUrl_ x) = x

data SpendCap = SpendCap
newtype SpendCap_ = SpendCap_ Text deriving (Show, Generic)
instance Field SpendCap where
	type FieldValue SpendCap = SpendCap_
	fieldName _ = "spend_cap"
	fieldLabel = SpendCap
unSpendCap_ :: SpendCap_ -> Text
unSpendCap_ (SpendCap_ x) = x

data Objective = Objective
newtype Objective_ = Objective_ ObjectiveADT deriving (Show, Generic)
instance Field Objective where
	type FieldValue Objective = Objective_
	fieldName _ = "objective"
	fieldLabel = Objective
unObjective_ :: Objective_ -> ObjectiveADT
unObjective_ (Objective_ x) = x

data BuyingType = BuyingType
newtype BuyingType_ = BuyingType_ BuyingTypeADT deriving (Show, Generic)
instance Field BuyingType where
	type FieldValue BuyingType = BuyingType_
	fieldName _ = "buying_type"
	fieldLabel = BuyingType
unBuyingType_ :: BuyingType_ -> BuyingTypeADT
unBuyingType_ (BuyingType_ x) = x

data Adaccounts = Adaccounts
newtype Adaccounts_ = Adaccounts_ (Vector Text) deriving (Show, Generic)
instance Field Adaccounts where
	type FieldValue Adaccounts = Adaccounts_
	fieldName _ = "adaccounts"
	fieldLabel = Adaccounts
unAdaccounts_ :: Adaccounts_ -> Vector Text
unAdaccounts_ (Adaccounts_ x) = x

data TimezoneId = TimezoneId
newtype TimezoneId_ = TimezoneId_ Int deriving (Show, Generic)
instance Field TimezoneId where
	type FieldValue TimezoneId = TimezoneId_
	fieldName _ = "timezone_id"
	fieldLabel = TimezoneId
unTimezoneId_ :: TimezoneId_ -> Int
unTimezoneId_ (TimezoneId_ x) = x

data IsNotificationsEnabled = IsNotificationsEnabled
newtype IsNotificationsEnabled_ = IsNotificationsEnabled_ Bool deriving (Show, Generic)
instance Field IsNotificationsEnabled where
	type FieldValue IsNotificationsEnabled = IsNotificationsEnabled_
	fieldName _ = "is_notifications_enabled"
	fieldLabel = IsNotificationsEnabled
unIsNotificationsEnabled_ :: IsNotificationsEnabled_ -> Bool
unIsNotificationsEnabled_ (IsNotificationsEnabled_ x) = x

data Partner = Partner
newtype Partner_ = Partner_ Text deriving (Show, Generic)
instance Field Partner where
	type FieldValue Partner = Partner_
	fieldName _ = "partner"
	fieldLabel = Partner
unPartner_ :: Partner_ -> Text
unPartner_ (Partner_ x) = x

data MediaAgency = MediaAgency
newtype MediaAgency_ = MediaAgency_ Text deriving (Show, Generic)
instance Field MediaAgency where
	type FieldValue MediaAgency = MediaAgency_
	fieldName _ = "media_agency"
	fieldLabel = MediaAgency
unMediaAgency_ :: MediaAgency_ -> Text
unMediaAgency_ (MediaAgency_ x) = x

data DisplaySequence = DisplaySequence
newtype DisplaySequence_ = DisplaySequence_ Int deriving (Show, Generic)
instance Field DisplaySequence where
	type FieldValue DisplaySequence = DisplaySequence_
	fieldName _ = "display_sequence"
	fieldLabel = DisplaySequence
unDisplaySequence_ :: DisplaySequence_ -> Int
unDisplaySequence_ (DisplaySequence_ x) = x

data CampaignGroupId = CampaignGroupId
newtype CampaignGroupId_ = CampaignGroupId_ Int deriving (Show, Generic)
instance Field CampaignGroupId where
	type FieldValue CampaignGroupId = CampaignGroupId_
	fieldName _ = "campaign_group_id"
	fieldLabel = CampaignGroupId
unCampaignGroupId_ :: CampaignGroupId_ -> Int
unCampaignGroupId_ (CampaignGroupId_ x) = x

data AdsetId = AdsetId
newtype AdsetId_ = AdsetId_ Int deriving (Show, Generic)
instance Field AdsetId where
	type FieldValue AdsetId = AdsetId_
	fieldName _ = "adset_id"
	fieldLabel = AdsetId
unAdsetId_ :: AdsetId_ -> Int
unAdsetId_ (AdsetId_ x) = x
instance A.FromJSON Id_
instance A.ToJSON Id_
instance A.FromJSON AccountId_
instance A.ToJSON AccountId_
instance A.FromJSON ExecutionOptions_
instance A.ToJSON ExecutionOptions_
instance A.FromJSON OptimizationGoal_
instance A.ToJSON OptimizationGoal_
instance A.FromJSON BidAmount_
instance A.ToJSON BidAmount_
instance A.FromJSON DailyImps_
instance A.ToJSON DailyImps_
instance A.FromJSON EndMinute_
instance A.ToJSON EndMinute_
instance A.FromJSON ApplicationId_
instance A.ToJSON ApplicationId_
instance A.FromJSON BillingEvent_
instance A.ToJSON BillingEvent_
instance A.FromJSON IsAutobid_
instance A.ToJSON IsAutobid_
instance A.FromJSON StartMinute_
instance A.ToJSON StartMinute_
instance A.FromJSON Days_
instance A.ToJSON Days_
instance A.FromJSON PixelId_
instance A.ToJSON PixelId_
instance A.FromJSON EndTime_
instance A.ToJSON EndTime_
instance A.FromJSON Name_
instance A.ToJSON Name_
instance A.FromJSON LifetimeImps_
instance A.ToJSON LifetimeImps_
instance A.FromJSON OfferId_
instance A.ToJSON OfferId_
instance A.FromJSON LifetimeBudget_
instance A.ToJSON LifetimeBudget_
instance A.FromJSON Redownload_
instance A.ToJSON Redownload_
instance A.FromJSON ObjectStoreUrl_
instance A.ToJSON ObjectStoreUrl_
instance A.FromJSON ProductSetId_
instance A.ToJSON ProductSetId_
instance A.FromJSON LifetimeFrequencyCap_
instance A.ToJSON LifetimeFrequencyCap_
instance A.FromJSON ProductCatalogId_
instance A.ToJSON ProductCatalogId_
instance A.FromJSON StartTime_
instance A.ToJSON StartTime_
instance A.FromJSON CreativeSequence_
instance A.ToJSON CreativeSequence_
instance A.FromJSON Targeting_
instance A.ToJSON Targeting_
instance A.FromJSON TimeStop_
instance A.ToJSON TimeStop_
instance A.FromJSON TimeStart_
instance A.ToJSON TimeStart_
instance A.FromJSON PageId_
instance A.ToJSON PageId_
instance A.FromJSON CampaignId_
instance A.ToJSON CampaignId_
instance A.FromJSON RtbFlag_
instance A.ToJSON RtbFlag_
instance A.FromJSON ConfiguredStatus_
instance A.ToJSON ConfiguredStatus_
instance A.FromJSON EffectiveStatus_
instance A.ToJSON EffectiveStatus_
instance A.FromJSON CreatedTime_
instance A.ToJSON CreatedTime_
instance A.FromJSON UpdatedTime_
instance A.ToJSON UpdatedTime_
instance A.FromJSON Hash_
instance A.ToJSON Hash_
instance A.FromJSON RunStatus_
instance A.ToJSON RunStatus_
instance A.FromJSON ActorImageUrl_
instance A.ToJSON ActorImageUrl_
instance A.FromJSON ActorImageHash_
instance A.ToJSON ActorImageHash_
instance A.FromJSON LinkOgId_
instance A.ToJSON LinkOgId_
instance A.FromJSON ActorName_
instance A.ToJSON ActorName_
instance A.FromJSON ObjectId_
instance A.ToJSON ObjectId_
instance A.FromJSON InstagramActorId_
instance A.ToJSON InstagramActorId_
instance A.FromJSON ActorId_
instance A.ToJSON ActorId_
instance A.FromJSON ThumbnailUrl_
instance A.ToJSON ThumbnailUrl_
instance A.FromJSON TemplateUrl_
instance A.ToJSON TemplateUrl_
instance A.FromJSON LinkUrl_
instance A.ToJSON LinkUrl_
instance A.FromJSON ObjectStorySpec_
instance A.ToJSON ObjectStorySpec_
instance A.FromJSON ObjectStoryId_
instance A.ToJSON ObjectStoryId_
instance A.FromJSON UrlTags_
instance A.ToJSON UrlTags_
instance A.FromJSON ImageHash_
instance A.ToJSON ImageHash_
instance A.FromJSON Title_
instance A.ToJSON Title_
instance A.FromJSON ObjectUrl_
instance A.ToJSON ObjectUrl_
instance A.FromJSON Body_
instance A.ToJSON Body_
instance A.FromJSON ImageUrl_
instance A.ToJSON ImageUrl_
instance A.FromJSON SpendCap_
instance A.ToJSON SpendCap_
instance A.FromJSON Objective_
instance A.ToJSON Objective_
instance A.FromJSON BuyingType_
instance A.ToJSON BuyingType_
instance A.FromJSON Adaccounts_
instance A.ToJSON Adaccounts_
instance A.FromJSON TimezoneId_
instance A.ToJSON TimezoneId_
instance A.FromJSON IsNotificationsEnabled_
instance A.ToJSON IsNotificationsEnabled_
instance A.FromJSON Partner_
instance A.ToJSON Partner_
instance A.FromJSON MediaAgency_
instance A.ToJSON MediaAgency_
instance A.FromJSON DisplaySequence_
instance A.ToJSON DisplaySequence_
instance A.FromJSON CampaignGroupId_
instance A.ToJSON CampaignGroupId_
instance A.FromJSON AdsetId_
instance A.ToJSON AdsetId_

instance ToBS Id_ where
	toBS (Id_ a) = toBS a

instance ToBS AccountId_ where
	toBS (AccountId_ a) = toBS a

instance ToBS ExecutionOptions_ where
	toBS (ExecutionOptions_ a) = toBS a

instance ToBS OptimizationGoal_ where
	toBS (OptimizationGoal_ a) = toBS a

instance ToBS BidAmount_ where
	toBS (BidAmount_ a) = toBS a

instance ToBS DailyImps_ where
	toBS (DailyImps_ a) = toBS a

instance ToBS EndMinute_ where
	toBS (EndMinute_ a) = toBS a

instance ToBS ApplicationId_ where
	toBS (ApplicationId_ a) = toBS a

instance ToBS BillingEvent_ where
	toBS (BillingEvent_ a) = toBS a

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

instance ToBS Targeting_ where
	toBS (Targeting_ a) = toBS a

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

instance ToBS ObjectStorySpec_ where
	toBS (ObjectStorySpec_ a) = toBS a

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

instance ToBS AdsetId_ where
	toBS (AdsetId_ a) = toBS a

id r = r `Rec.get` Id
account_id r = r `Rec.get` AccountId
execution_options r = r `Rec.get` ExecutionOptions
optimization_goal r = r `Rec.get` OptimizationGoal
bid_amount r = r `Rec.get` BidAmount
daily_imps r = r `Rec.get` DailyImps
end_minute r = r `Rec.get` EndMinute
application_id r = r `Rec.get` ApplicationId
billing_event r = r `Rec.get` BillingEvent
is_autobid r = r `Rec.get` IsAutobid
start_minute r = r `Rec.get` StartMinute
days r = r `Rec.get` Days
pixel_id r = r `Rec.get` PixelId
end_time r = r `Rec.get` EndTime
name r = r `Rec.get` Name
lifetime_imps r = r `Rec.get` LifetimeImps
offer_id r = r `Rec.get` OfferId
lifetime_budget r = r `Rec.get` LifetimeBudget
redownload r = r `Rec.get` Redownload
object_store_url r = r `Rec.get` ObjectStoreUrl
product_set_id r = r `Rec.get` ProductSetId
lifetime_frequency_cap r = r `Rec.get` LifetimeFrequencyCap
product_catalog_id r = r `Rec.get` ProductCatalogId
start_time r = r `Rec.get` StartTime
creative_sequence r = r `Rec.get` CreativeSequence
targeting r = r `Rec.get` Targeting
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
object_story_spec r = r `Rec.get` ObjectStorySpec
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
adset_id r = r `Rec.get` AdsetId