module Facebook.Gen.CodeGenStrTypes where

import Data.Text
import Data.Monoid

-- include this file in the generated Types.hs
oldTypesImport :: Text
oldTypesImport =
    "import Data.Text (Text, pack, unpack)\n\
    \import Data.Typeable (Typeable)\n\
    \import GHC.Generics\n\
    \import Data.Aeson\n\
    \import Data.Aeson.Types\n\
    \import Control.Applicative\n\
    \import System.Locale hiding (defaultTimeLocale, rfc822DateFormat)\n\
    \import Control.Monad\n\
    \import qualified Data.ByteString as BS\n\
    \import qualified Data.ByteString.Lazy as BSL\n\
    \import qualified Data.Text.Encoding as TE\n\
    \import Facebook.Object.Marketing.Utility hiding (toBS)\n\
    \import Facebook.Object.Marketing.TargetingSpecs\n\
    \import Text.Read (readMaybe)\n"

newTypes :: Text
newTypes =
    "data ConfigureStatusADT = ACTIVE_ | PAUSED_ | DELETED_ | ARCHIVED_ deriving Generic\n\
    \instance Show ConfigureStatusADT where\n\
    \\t show ACTIVE_ = \"ACTIVE\"\n\
    \\t show PAUSED_ = \"PAUSED\"\n\
    \\t show DELETED_ = \"DELETED\"\n\
    \\t show ARCHIVED_ = \"ARCHIVED\"\n\
    \instance ToJSON ConfigureStatusADT where\n\
    \\ttoJSON ACTIVE_ = String \"ACTIVE\"\n\
    \\ttoJSON PAUSED_ = String \"PAUSED\"\n\
    \\ttoJSON DELETED_ = String \"DELETED\"\n\
    \\ttoJSON ARCHIVED_ = String \"ARCHIVED\"\n\
    \instance FromJSON ConfigureStatusADT where\n\
    \\tparseJSON (String \"ACTIVE\") = pure ACTIVE_\n\
    \\tparseJSON (String \"PAUSED\") = pure PAUSED_\n\
    \\tparseJSON (String \"DELETED\") = pure DELETED_\n\
    \\tparseJSON (String \"ARCHIVED\") = pure ARCHIVED_\n\
    \instance ToBS ConfigureStatusADT\n\
    \data EffectiveStatusADT = ACTIVE | PAUSED | DELETED | ARCHIVED | PENDING_REVIEW | DISAPPROVED | PREAPPROVED | PENDING_BILLING_INFO | CAMPAIGN_PAUSED | ADSET_PAUSED deriving (Show, Generic)\n\
    \instance FromJSON EffectiveStatusADT\n\
    \instance ToJSON EffectiveStatusADT\n\
    \instance ToBS EffectiveStatusADT\n"
    <> execOption <> optGoal <> bidType <> callToActionType
    <> runStatus <> objective <> buyingType <> deleteStrategy
    <> billingEvent <> objectStorySpec <> adCreativeLinkData <> carouselChildren
    <> creativeADT <> callToAction <> genericRetType <> genericIdRetType

creativeADT =
    "data AdCreativeADT = AdCreativeADT {\n\
    \\tcreative_id  :: Text\n\
    \\t} deriving (Show, Generic)\n"
    <>
    "instance FromJSON AdCreativeADT\n\
    \instance ToJSON AdCreativeADT\n"
    <>
    "instance ToBS AdCreativeADT where\n\
    \\ttoBS = toBS . toJSON\n"

objectStorySpec =
    "data ObjectStorySpecADT = ObjectStorySpecADT {\n\
    \\t\tlinkData  :: AdCreativeLinkData,\n\
    \\t\tstoryPageId  :: FBPageId,\n\
    \\t\tigId  :: Maybe IgId\n\
    \\t} deriving (Show, Generic)\n"
    <>
    "newtype FBPageId = FBPageId Text deriving (Show, Generic)\n\
    \instance FromJSON FBPageId\n\
    \newtype IgId = IgId Text deriving (Show, Generic)\n\
    \instance FromJSON IgId\n"
    <>
    "instance ToJSON ObjectStorySpecADT where\n\
    \\ttoJSON (ObjectStorySpecADT ld (FBPageId pi) Nothing) =\n\
    \\t  object [ \"link_data\" .= ld,\n\
    \\t           \"page_id\" .= pi] \n\
    \\ttoJSON (ObjectStorySpecADT ld (FBPageId pi) (Just (IgId ig))) =\n\
    \\t  object [ \"link_data\" .= ld,\n\
    \\t           \"page_id\" .= pi, \n\
    \\t           \"instagram_actor_id\" .= ig] \n"
    <>
    "instance FromJSON ObjectStorySpecADT where\n\
    \\tparseJSON (Object v) =\n\
    \\t ObjectStorySpecADT <$> v .: \"link_data\"\n\
    \\t                    <*> v .: \"page_id\"\n\
    \\t                    <*> v .:? \"instagram_actor_id\"\n"
    <>
    "instance ToBS ObjectStorySpecADT where\n\
    \\ttoBS a = toBS $ toJSON a\n" -- FIXME Maybe this should be the default implementation?

callToAction =
    "data CallToActionValue = CallToActionValue {\n\
    \\tctav_link, ctav_link_caption :: Text\n\
    \\t} deriving (Show, Generic)\n"
    <>
    "instance ToJSON CallToActionValue where\n\
    \\ttoJSON = genericToJSON defaultOptions {fieldLabelModifier = drop $ length (\"ctav_\" :: String)}\n"
    <>
    "instance FromJSON CallToActionValue where\n\
    \\tparseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop $ length (\"ctav_\" :: String)}\n"
    <>
    "data CallToActionADT = CallToActionADT {\n\
    \\t\tcta_type :: CallToActionTypeADT,\n\
    \\t\tcta_value :: CallToActionValue\n\
    \\t} deriving (Show, Generic)\n"
    <>
    "instance ToJSON CallToActionADT where\n\
    \\ttoJSON = genericToJSON defaultOptions {fieldLabelModifier = drop $ length (\"cta_\" :: String)}\n"
    <>
    "instance FromJSON CallToActionADT where\n\
    \\tparseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop $ length (\"cta_\" :: String)}\n"

carouselChildren =
    "type CarouselChildren = [CarouselChild]\n"
    <>
    "data CarouselChild = CarouselChild {\n\
    \\t\tname_car_child :: Text,\n\
    \\t\timageHash_car_child ::  Hash_,\n\
    \\t\tlink_car_child :: Text,\n\
    \\tdescription_car_child  :: Maybe Text}\n\
    \\tderiving (Show, Generic)\n"
    <>
    "instance ToJSON CarouselChild where\n\
    \\ttoJSON (CarouselChild n i l (Just d)) =\n\
    \\t  object [ \"name\" .= n,\n\
    \\t           \"image_hash\" .= i,\n\
    \\t           \"link\" .= l,\n\
    \\t           \"description\" .= d]\n"
    <>
    "instance FromJSON CarouselChild where\n\
    \\tparseJSON (Object v) =\n\
    \\t CarouselChild <$> v .: \"name\"\n\
    \\t               <*> v .: \"image_hash\"\n\
    \\t               <*> v .: \"link\"\n\
    \\t               <*> v .:? \"description\"\n"
    <>
    "instance ToBS CarouselChild where\n\
    \\ttoBS a = toBS $ toJSON a\n" -- FIXME Maybe this should be the default implementation?

adCreativeLinkData =
    "data AdCreativeLinkData = AdCreativeLinkData {\n\
    \\t\tcaption  :: Text,\n\
    \\t\timageHash ::  Hash_,\n\
    \\t\tlink, message :: Text,\n\
    \\t\tdescription  :: Maybe Text,\n\
    \\t\tcall_to_action :: Maybe CallToActionADT}\n\
    \\t| CreativeCarouselData {\n\
    \\t\tcaption_carousel, message_carousel :: Text,\n\
    \\t\tchild_attachments ::  CarouselChildren,\n\
    \\t\tlink :: Text }\n\
    \\tderiving (Show, Generic)\n"
    <>
    "instance ToJSON AdCreativeLinkData where\n\
    \\ttoJSON (AdCreativeLinkData c i l m (Just d) (Just cta)) =\n\
    \\t  object [ \"caption\" .= c,\n\
    \\t           \"image_hash\" .= i,\n\
    \\t           \"link\" .= l,\n\
    \\t           \"message\" .= m]\n\
    \\t           --\"description\" .= d,\n\
    \\t           --\"call_to_action\" .= cta]\n\
    \\ttoJSON (CreativeCarouselData c m cs l) =\n\
    \\t  object [ \"caption\" .= c,\n\
    \\t           \"message\" .= m,\n\
    \\t           \"child_attachments\" .= toJSON cs,\n\
    \\t           \"link\" .= l]\n"
    <>
    "instance FromJSON AdCreativeLinkData where\n\
    \\tparseJSON (Object v) = do\n\
    \\t typ <- v .:? \"child_attachments\" :: Parser (Maybe CarouselChildren)\n\
    \\t case typ of\n\
    \\t\t Nothing -> parseAdCreativeLinkData v\n\
    \\t\t Just _  -> parseCreativeCarouselData v\n"
    <>
    "\nparseAdCreativeLinkData v =\n\
    \\t AdCreativeLinkData <$> v .: \"caption\"\n\
    \\t                    <*> v .: \"image_hash\"\n\
    \\t                    <*> v .: \"link\"\n\
    \\t                    <*> v .: \"message\"\n\
    \\t                    <*> v .:? \"description\"\n\
    \\t                    <*> v .:? \"call_to_action\"\n"
    <>
    "\nparseCreativeCarouselData v =\n\
    \\t CreativeCarouselData <$> v .: \"caption\"\n\
    \\t                      <*> v .: \"message\"\n\
    \\t                      <*> v .: \"child_attachments\"\n\
    \\t                      <*> v .: \"link\"\n"
    <>
    "instance ToBS AdCreativeLinkData where\n\
    \\ttoBS a = toBS $ toJSON a\n" -- FIXME Maybe this should be the default implementation?

billingEvent =
    "data BillingEventADT = APP_INSTALLS_ | CLICKS_ | IMPRESSIONS_ | LINK_CLICKS_ | OFFER_CLAIMS_ | PAGE_LIKES_ | \
    \POST_ENGAGEMENT_ | VIDEO_VIEWS_\n" <>
    "instance Show BillingEventADT where\n\
    \\t show APP_INSTALLS_ = \"APP_INSTALLS\"\n\
    \\t show CLICKS_ = \"CLICKS\"\n\
    \\t show IMPRESSIONS_ = \"IMPRESSIONS\"\n\
    \\t show LINK_CLICKS_ = \"LINK_CLICKS\"\n\
    \\t show OFFER_CLAIMS_ = \"OFFER_CLAIMS\"\n\
    \\t show PAGE_LIKES_ = \"PAGE_LIKES\"\n\
    \\t show POST_ENGAGEMENT_ = \"POST_ENGAGEMENT\"\n\
    \\t show VIDEO_VIEWS_ = \"VIDEO_VIEWS\"\n" <>
    "instance ToBS BillingEventADT\n"
    <>
    "instance ToJSON BillingEventADT where\n\
    \\ttoJSON = toJSON . show\n\
    \instance FromJSON BillingEventADT where\n\
    \\tparseJSON (String \"APP_INSTALLS\") = pure APP_INSTALLS_\n\
    \\tparseJSON (String \"IMPRESSIONS\") = pure IMPRESSIONS_\n\
    \\tparseJSON (String \"CLICKS\") = pure CLICKS_\n\
    \\tparseJSON (String \"LINK_CLICKS\") = pure LINK_CLICKS_\n\
    \\tparseJSON (String \"OFFER_CLAIMS\") = pure OFFER_CLAIMS_\n\
    \\tparseJSON (String \"PAGE_LIKES\") = pure PAGE_LIKES_\n\
    \\tparseJSON (String \"POST_ENGAGEMENT\") = pure POST_ENGAGEMENT_\n\
    \\tparseJSON (String \"VIDEO_VIEWS\") = pure VIDEO_VIEWS_\n"

deleteStrategy =
    "data DeleteStrategyADT = DELETE_ANY | DELETE_OLDEST | DELETE_ARCHIVED_BEFORE deriving (Show, Generic)\n\
    \instance FromJSON DeleteStrategyADT\n\
    \instance ToJSON DeleteStrategyADT\n\
    \instance ToBS DeleteStrategyADT\n"

buyingType =
    "data BuyingTypeADT = AUCTION | RESERVED deriving (Show, Generic)\n\
    \instance FromJSON BuyingTypeADT\n\
    \instance ToJSON BuyingTypeADT\n\
    \instance ToBS BuyingTypeADT\n"

objective =
    "data ObjectiveADT = OBJ_BRAND_AWARENESS | OBJ_CANVAS_APP_ENGAGEMENT | OBJ_CANVAS_APP_INSTALLS | \
    \OBJ_CONVERSIONS | OBJ_EVENT_RESPONSES | OBJ_EXTERNAL | OBJ_LEAD_GENERATION | OBJ_LINK_CLICKS | OBJ_LOCAL_AWARENESS | \
    \OBJ_MOBILE_APP_ENGAGEMENT | OBJ_MOBILE_APP_INSTALLS | OBJ_OFFER_CLAIMS | OBJ_PAGE_LIKES | OBJ_POST_ENGAGEMENT | \
    \OBJ_PRODUCT_CATALOG_SALES | OBJ_VIDEO_VIEWS | OBJ_NONE\n"
    <>
    "instance Show ObjectiveADT where\n\
    \\tshow OBJ_BRAND_AWARENESS = \"BRAND_AWARENESS\"\n\
    \\tshow OBJ_CANVAS_APP_ENGAGEMENT = \"CANVAS_APP_ENGAGEMENT\"\n\
    \\tshow OBJ_CANVAS_APP_INSTALLS = \"CANVAS_APP_INSTALLS\"\n\
    \\tshow OBJ_CONVERSIONS = \"CONVERSIONS\"\n\
    \\tshow OBJ_EVENT_RESPONSES = \"EVENT_RESPONSES\"\n\
    \\tshow OBJ_EXTERNAL = \"EXTERNAL\"\n\
    \\tshow OBJ_LEAD_GENERATION = \"LEAD_GENERATION\"\n\
    \\tshow OBJ_LINK_CLICKS = \"LINK_CLICKS\"\n\
    \\tshow OBJ_LOCAL_AWARENESS = \"LOCAL_AWARENESS\"\n\
    \\tshow OBJ_MOBILE_APP_ENGAGEMENT = \"MOBILE_APP_ENGAGEMENT\"\n\
    \\tshow OBJ_MOBILE_APP_INSTALLS = \"MOBILE_APP_INSTALLS\"\n\
    \\tshow OBJ_OFFER_CLAIMS = \"OFFER_CLAIMS\"\n\
    \\tshow OBJ_PAGE_LIKES = \"PAGE_LIKES\"\n\
    \\tshow OBJ_POST_ENGAGEMENT = \"POST_ENGAGEMENT\"\n\
    \\tshow OBJ_PRODUCT_CATALOG_SALES = \"PRODUCT_CATALOG_SALES\"\n\
    \\tshow OBJ_NONE = \"NONE\"\n\
    \\tshow OBJ_VIDEO_VIEWS = \"VIDEO_VIEWS\"\n"
    <>
    "instance ToBS ObjectiveADT\n"
    <>
    "instance ToJSON ObjectiveADT where\n\
    \\ttoJSON = toJSON . show\n\
    \instance FromJSON ObjectiveADT where\n\
    \\tparseJSON (String \"BRAND_AWARENESS\") = pure OBJ_BRAND_AWARENESS\n\
    \\tparseJSON (String \"CANVAS_APP_ENGAGEMENT\") = pure OBJ_CANVAS_APP_ENGAGEMENT\n\
    \\tparseJSON (String \"CANVAS_APP_INSTALLS\") = pure OBJ_CANVAS_APP_INSTALLS\n\
    \\tparseJSON (String \"CONVERSIONS\") = pure OBJ_CONVERSIONS\n\
    \\tparseJSON (String \"EVENT_RESPONSES\") = pure OBJ_EVENT_RESPONSES\n\
    \\tparseJSON (String \"EXTERNAL\") = pure OBJ_EXTERNAL\n\
    \\tparseJSON (String \"LEAD_GENERATION\") = pure OBJ_LEAD_GENERATION\n\
    \\tparseJSON (String \"LINK_CLICKS\") = pure OBJ_LINK_CLICKS\n\
    \\tparseJSON (String \"LOCAL_AWARENESS\") = pure OBJ_LOCAL_AWARENESS\n\
    \\tparseJSON (String \"MOBILE_APP_ENGAGEMENT\") = pure OBJ_MOBILE_APP_ENGAGEMENT\n\
    \\tparseJSON (String \"MOBILE_APP_INSTALLS\") = pure OBJ_MOBILE_APP_INSTALLS\n\
    \\tparseJSON (String \"OFFER_CLAIMS\") = pure OBJ_OFFER_CLAIMS\n\
    \\tparseJSON (String \"PAGE_LIKES\") = pure OBJ_PAGE_LIKES\n\
    \\tparseJSON (String \"POST_ENGAGEMENT\") = pure OBJ_POST_ENGAGEMENT\n\
    \\tparseJSON (String \"PRODUCT_CATALOG_SALES\") = pure OBJ_PRODUCT_CATALOG_SALES\n\
    \\tparseJSON (String \"NONE\") = pure OBJ_NONE\n\
    \\tparseJSON (String \"VIDEO_VIEWS\") = pure OBJ_VIDEO_VIEWS\n"

runStatus =
    "data RunStatusADT = RS_ACTIVE | RS_DELETED\n\
    \instance Show RunStatusADT where\n\
    \\tshow RS_ACTIVE = \"ACTIVE\"\n\
    \\tshow RS_DELETED = \"DELETED\"\n\
    \instance FromJSON RunStatusADT where\n\
    \\tparseJSON (String \"ACTIVE\") = pure RS_ACTIVE\n\
    \\tparseJSON (String \"DELETED\") = pure RS_DELETED\n\
    \instance ToBS RunStatusADT\n"

callToActionType =
    "data CallToActionTypeADT = OPEN_LINK | LIKE_PAGE | SHOP_NOW | PLAY_GAME | INSTALL_APP | USE_APP |\
    \INSTALL_MOBILE_APP | USE_MOBILE_APP | BOOK_TRAVEL | LISTEN_MUSIC | WATCH_VIDEO | LEARN_MORE |\
    \SIGN_UP | DOWNLOAD | WATCH_MORE | NO_BUTTON | CALL_NOW | BUY_NOW | GET_OFFER | GET_DIRECTIONS |\
    \MESSAGE_PAGE | SUBSCRIBE | DONATE_NOW | GET_QUOTE | CONTACT_US | RECORD_NOW | OPEN_MOVIES deriving (Show, Generic)\n\
    \instance FromJSON CallToActionTypeADT\n\
    \instance ToJSON CallToActionTypeADT\n\
    \instance ToBS CallToActionTypeADT\n"

bidType =
    "\ndata BidTypeADT = CPC | CPM | MULTI_PREMIUM | ABSOLUTE_OCPM | CPA deriving (Show, Generic)\n\
    \instance FromJSON BidTypeADT\n\
    \instance ToJSON BidTypeADT\n\
    \instance ToBS BidTypeADT\n"

execOption =
    "\ndata ExecOption = VALIDATE_ONLY | INCLUDE_WARNINGS deriving (Show, Generic)\n\
    \instance ToJSON ExecOption\n\
    \instance FromJSON ExecOption\n\
    \instance ToBS ExecOption\n"

optGoal =
    "data OptGoal = NONE | APP_INSTALLS | CLICKS | ENGAGED_USERS | EXTERNAL \
    \| EVENT_RESPONSES | IMPRESSIONS | LEAD_GENERATION | LINK_CLICKS | \
    \OFFER_CLAIMS | OFFSITE_CONVERSIONS | PAGE_ENGAGEMENT | PAGE_LIKES | \
    \POST_ENGAGEMENT | REACH | SOCIAL_IMPRESSIONS | VIDEO_VIEWS deriving (Show, Generic)\n\
    \instance FromJSON OptGoal\n\
    \instance ToJSON OptGoal\n\
    \instance ToBS OptGoal\n"

genericRetType =
    "\ndata Success = Success {\n\
    \\tsuccess :: Bool\n\
    \\t} deriving (Show, Generic)\n\
    \instance FromJSON Success\n"

--genericIdRetType =
--    "\ndata SuccessId = SuccessId {\n\
--    \\t   id_ :: Text\n\
--    \\t,  success_ :: Bool\n\
--    \\t} deriving (Show, Generic)\n\
--    \instance FromJSON SuccessId where \n\
--    \\tparseJSON (Object v) =\n\
--    \\t\t\tSuccessId <$> v .: \"id\"\n\
--    \\t\t\t          <*> v .: \"success\"\n"

genericIdRetType =
    "\ndata SuccessId = SuccessId {\n\
    \\t   id_ :: Text\n\
    \\t} deriving (Show, Generic)\n\
    \instance FromJSON SuccessId where \n\
    \\tparseJSON (Object v) =\n\
    \\t\t\tSuccessId <$> v .: \"id\"\n"

oldTypes :: Text
oldTypes =
    "newtype Money = Money {getMoneyInPennies :: Int} deriving (Eq, Ord, Show, Read, Typeable, Generic, Num)\n\n\

    \instance FromJSON Money where\n\
      \\tparseJSON (String val) =\n\
      \\t\tcase readMaybe $ unpack val of\n\
            \\t\t\tJust x -> return $ Money x\n\
            \\t\t\tNothing -> fail $ \"Money parser string: \"++show (unpack val)\n\
                \\t\twhere\n\
                  \\t\t\ttextToBSL :: Text -> BSL.ByteString\n\
                  \\t\t\ttextToBSL = BSL.fromStrict . TE.encodeUtf8\n\
      \\tparseJSON n@(Number _) = do\n\
          \\t\tx <- parseJSON n\n\
          \\t\treturn $ Money x\n\
      \\tparseJSON v = fail $ \"Money parser value: \"++show v\n\n\

    \instance ToJSON Money where\n\
      \\ttoJSON (Money cents) = String $ pack $ show cents\n\n\

    \data FbNumeric = FbStringNumeric Text\n\
                   \\t\t| FbIntegerNumeric Int\n\
                                 \\tderiving (Show, Read, Generic)\n\

    \instance FromJSON FbNumeric\n\
    \instance ToJSON FbNumeric\n\n\

    \data FBMObjectCreated = FBMObjectCreated\n\
      \\t{ oc_id :: Text\n\
      \\t, oc_success :: Bool\n\
      \\t} deriving (Show, Generic)\n\

    \instance FromJSON FBMObjectCreated where\n\
      \\tparseJSON = parseJSONWithPrefix \"oc_\"\n\n\

    \class ToFbText a where\n\
      \\ttoFbText :: a -> Text\n"
