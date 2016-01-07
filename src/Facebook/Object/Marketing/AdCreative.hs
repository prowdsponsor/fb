{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.AdCreative where

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
import Facebook.Object.Marketing.Types

data ActionSpec = ActionSpec
newtype ActionSpec_ = ActionSpec_ (Vector Int) deriving (Show, Generic)
instance Field ActionSpec where
	type FieldValue ActionSpec = ActionSpec_
	fieldName _ = "action_spec"
	fieldLabel = ActionSpec
unActionSpec_ :: ActionSpec_ -> Vector Int
unActionSpec_ (ActionSpec_ x) = x

data CallToAction = CallToAction
newtype CallToAction_ = CallToAction_ A.Value deriving (Show, Generic)
instance Field CallToAction where
	type FieldValue CallToAction = CallToAction_
	fieldName _ = "call_to_action"
	fieldLabel = CallToAction
unCallToAction_ :: CallToAction_ -> A.Value
unCallToAction_ (CallToAction_ x) = x

data PlacePageSetId = PlacePageSetId
newtype PlacePageSetId_ = PlacePageSetId_ Text deriving (Show, Generic)
instance Field PlacePageSetId where
	type FieldValue PlacePageSetId = PlacePageSetId_
	fieldName _ = "place_page_set_id"
	fieldLabel = PlacePageSetId
unPlacePageSetId_ :: PlacePageSetId_ -> Text
unPlacePageSetId_ (PlacePageSetId_ x) = x

data Value = Value
newtype Value_ = Value_ A.Value deriving (Show, Generic)
instance Field Value where
	type FieldValue Value = Value_
	fieldName _ = "value"
	fieldLabel = Value
unValue_ :: Value_ -> A.Value
unValue_ (Value_ x) = x

data ImageFile = ImageFile
newtype ImageFile_ = ImageFile_ Text deriving (Show, Generic)
instance Field ImageFile where
	type FieldValue ImageFile = ImageFile_
	fieldName _ = "image_file"
	fieldLabel = ImageFile
unImageFile_ :: ImageFile_ -> Text
unImageFile_ (ImageFile_ x) = x

data FollowRedirect = FollowRedirect
newtype FollowRedirect_ = FollowRedirect_ Bool deriving (Show, Generic)
instance Field FollowRedirect where
	type FieldValue FollowRedirect = FollowRedirect_
	fieldName _ = "follow_redirect"
	fieldLabel = FollowRedirect
unFollowRedirect_ :: FollowRedirect_ -> Bool
unFollowRedirect_ (FollowRedirect_ x) = x

data ObjectInstagramId = ObjectInstagramId
newtype ObjectInstagramId_ = ObjectInstagramId_ Int deriving (Show, Generic)
instance Field ObjectInstagramId where
	type FieldValue ObjectInstagramId = ObjectInstagramId_
	fieldName _ = "object_instagram_id"
	fieldLabel = ObjectInstagramId
unObjectInstagramId_ :: ObjectInstagramId_ -> Int
unObjectInstagramId_ (ObjectInstagramId_ x) = x

data VideoId = VideoId
newtype VideoId_ = VideoId_ Int deriving (Show, Generic)
instance Field VideoId where
	type FieldValue VideoId = VideoId_
	fieldName _ = "video_id"
	fieldLabel = VideoId
unVideoId_ :: VideoId_ -> Int
unVideoId_ (VideoId_ x) = x

data Image = Image
newtype Image_ = Image_ Text deriving (Show, Generic)
instance Field Image where
	type FieldValue Image = Image_
	fieldName _ = "image"
	fieldLabel = Image
unImage_ :: Image_ -> Text
unImage_ (Image_ x) = x

data AppLink = AppLink
newtype AppLink_ = AppLink_ Text deriving (Show, Generic)
instance Field AppLink where
	type FieldValue AppLink = AppLink_
	fieldName _ = "app_link"
	fieldLabel = AppLink
unAppLink_ :: AppLink_ -> Text
unAppLink_ (AppLink_ x) = x

data LinkCaption = LinkCaption
newtype LinkCaption_ = LinkCaption_ Text deriving (Show, Generic)
instance Field LinkCaption where
	type FieldValue LinkCaption = LinkCaption_
	fieldName _ = "link_caption"
	fieldLabel = LinkCaption
unLinkCaption_ :: LinkCaption_ -> Text
unLinkCaption_ (LinkCaption_ x) = x

data GetMovieShowtimes = GetMovieShowtimes
newtype GetMovieShowtimes_ = GetMovieShowtimes_ Bool deriving (Show, Generic)
instance Field GetMovieShowtimes where
	type FieldValue GetMovieShowtimes = GetMovieShowtimes_
	fieldName _ = "get_movie_showtimes"
	fieldLabel = GetMovieShowtimes
unGetMovieShowtimes_ :: GetMovieShowtimes_ -> Bool
unGetMovieShowtimes_ (GetMovieShowtimes_ x) = x

data Leadgen = Leadgen
newtype Leadgen_ = Leadgen_ A.Value deriving (Show, Generic)
instance Field Leadgen where
	type FieldValue Leadgen = Leadgen_
	fieldName _ = "leadgen"
	fieldLabel = Leadgen
unLeadgen_ :: Leadgen_ -> A.Value
unLeadgen_ (Leadgen_ x) = x

data LinkDescription = LinkDescription
newtype LinkDescription_ = LinkDescription_ Text deriving (Show, Generic)
instance Field LinkDescription where
	type FieldValue LinkDescription = LinkDescription_
	fieldName _ = "link_description"
	fieldLabel = LinkDescription
unLinkDescription_ :: LinkDescription_ -> Text
unLinkDescription_ (LinkDescription_ x) = x

data Sponsorship = Sponsorship
newtype Sponsorship_ = Sponsorship_ A.Value deriving (Show, Generic)
instance Field Sponsorship where
	type FieldValue Sponsorship = Sponsorship_
	fieldName _ = "sponsorship"
	fieldLabel = Sponsorship
unSponsorship_ :: Sponsorship_ -> A.Value
unSponsorship_ (Sponsorship_ x) = x

data Application = Application
newtype Application_ = Application_ Text deriving (Show, Generic)
instance Field Application where
	type FieldValue Application = Application_
	fieldName _ = "application"
	fieldLabel = Application
unApplication_ :: Application_ -> Text
unApplication_ (Application_ x) = x

data VideoAnnotation = VideoAnnotation
newtype VideoAnnotation_ = VideoAnnotation_ A.Value deriving (Show, Generic)
instance Field VideoAnnotation where
	type FieldValue VideoAnnotation = VideoAnnotation_
	fieldName _ = "video_annotation"
	fieldLabel = VideoAnnotation
unVideoAnnotation_ :: VideoAnnotation_ -> A.Value
unVideoAnnotation_ (VideoAnnotation_ x) = x

data StartTimeInSec = StartTimeInSec
newtype StartTimeInSec_ = StartTimeInSec_ Int deriving (Show, Generic)
instance Field StartTimeInSec where
	type FieldValue StartTimeInSec = StartTimeInSec_
	fieldName _ = "start_time_in_sec"
	fieldLabel = StartTimeInSec
unStartTimeInSec_ :: StartTimeInSec_ -> Int
unStartTimeInSec_ (StartTimeInSec_ x) = x

data PolicyUrl = PolicyUrl
newtype PolicyUrl_ = PolicyUrl_ Text deriving (Show, Generic)
instance Field PolicyUrl where
	type FieldValue PolicyUrl = PolicyUrl_
	fieldName _ = "policy_url"
	fieldLabel = PolicyUrl
unPolicyUrl_ :: PolicyUrl_ -> Text
unPolicyUrl_ (PolicyUrl_ x) = x

data Link = Link
newtype Link_ = Link_ Text deriving (Show, Generic)
instance Field Link where
	type FieldValue Link = Link_
	fieldName _ = "link"
	fieldLabel = Link
unLink_ :: Link_ -> Text
unLink_ (Link_ x) = x

data ProductLink = ProductLink
newtype ProductLink_ = ProductLink_ Text deriving (Show, Generic)
instance Field ProductLink where
	type FieldValue ProductLink = ProductLink_
	fieldName _ = "product_link"
	fieldLabel = ProductLink
unProductLink_ :: ProductLink_ -> Text
unProductLink_ (ProductLink_ x) = x

data EndTimeInSec = EndTimeInSec
newtype EndTimeInSec_ = EndTimeInSec_ Int deriving (Show, Generic)
instance Field EndTimeInSec where
	type FieldValue EndTimeInSec = EndTimeInSec_
	fieldName _ = "end_time_in_sec"
	fieldLabel = EndTimeInSec
unEndTimeInSec_ :: EndTimeInSec_ -> Int
unEndTimeInSec_ (EndTimeInSec_ x) = x

data Page = Page
newtype Page_ = Page_ Text deriving (Show, Generic)
instance Field Page where
	type FieldValue Page = Page_
	fieldName _ = "page"
	fieldLabel = Page
unPage_ :: Page_ -> Text
unPage_ (Page_ x) = x

data FallbackTestUrl = FallbackTestUrl
newtype FallbackTestUrl_ = FallbackTestUrl_ Text deriving (Show, Generic)
instance Field FallbackTestUrl where
	type FieldValue FallbackTestUrl = FallbackTestUrl_
	fieldName _ = "fallback_test_url"
	fieldLabel = FallbackTestUrl
unFallbackTestUrl_ :: FallbackTestUrl_ -> Text
unFallbackTestUrl_ (FallbackTestUrl_ x) = x

data FollowUpActionUrl = FollowUpActionUrl
newtype FollowUpActionUrl_ = FollowUpActionUrl_ Text deriving (Show, Generic)
instance Field FollowUpActionUrl where
	type FieldValue FollowUpActionUrl = FollowUpActionUrl_
	fieldName _ = "follow_up_action_url"
	fieldLabel = FollowUpActionUrl
unFollowUpActionUrl_ :: FollowUpActionUrl_ -> Text
unFollowUpActionUrl_ (FollowUpActionUrl_ x) = x

data SplitFlowUsePost = SplitFlowUsePost
newtype SplitFlowUsePost_ = SplitFlowUsePost_ Bool deriving (Show, Generic)
instance Field SplitFlowUsePost where
	type FieldValue SplitFlowUsePost = SplitFlowUsePost_
	fieldName _ = "split_flow_use_post"
	fieldLabel = SplitFlowUsePost
unSplitFlowUsePost_ :: SplitFlowUsePost_ -> Bool
unSplitFlowUsePost_ (SplitFlowUsePost_ x) = x

data FollowUpTitle = FollowUpTitle
newtype FollowUpTitle_ = FollowUpTitle_ Text deriving (Show, Generic)
instance Field FollowUpTitle where
	type FieldValue FollowUpTitle = FollowUpTitle_
	fieldName _ = "follow_up_title"
	fieldLabel = FollowUpTitle
unFollowUpTitle_ :: FollowUpTitle_ -> Text
unFollowUpTitle_ (FollowUpTitle_ x) = x

data NeedSplitFlow = NeedSplitFlow
newtype NeedSplitFlow_ = NeedSplitFlow_ Bool deriving (Show, Generic)
instance Field NeedSplitFlow where
	type FieldValue NeedSplitFlow = NeedSplitFlow_
	fieldName _ = "need_split_flow"
	fieldLabel = NeedSplitFlow
unNeedSplitFlow_ :: NeedSplitFlow_ -> Bool
unNeedSplitFlow_ (NeedSplitFlow_ x) = x

data TcpaCompliant = TcpaCompliant
newtype TcpaCompliant_ = TcpaCompliant_ Bool deriving (Show, Generic)
instance Field TcpaCompliant where
	type FieldValue TcpaCompliant = TcpaCompliant_
	fieldName _ = "tcpa_compliant"
	fieldLabel = TcpaCompliant
unTcpaCompliant_ :: TcpaCompliant_ -> Bool
unTcpaCompliant_ (TcpaCompliant_ x) = x

data FollowUpActionText = FollowUpActionText
newtype FollowUpActionText_ = FollowUpActionText_ Text deriving (Show, Generic)
instance Field FollowUpActionText where
	type FieldValue FollowUpActionText = FollowUpActionText_
	fieldName _ = "follow_up_action_text"
	fieldLabel = FollowUpActionText
unFollowUpActionText_ :: FollowUpActionText_ -> Text
unFollowUpActionText_ (FollowUpActionText_ x) = x

data LandingPageCta = LandingPageCta
newtype LandingPageCta_ = LandingPageCta_ Text deriving (Show, Generic)
instance Field LandingPageCta where
	type FieldValue LandingPageCta = LandingPageCta_
	fieldName _ = "landing_page_cta"
	fieldLabel = LandingPageCta
unLandingPageCta_ :: LandingPageCta_ -> Text
unLandingPageCta_ (LandingPageCta_ x) = x

data LinkTitle = LinkTitle
newtype LinkTitle_ = LinkTitle_ Text deriving (Show, Generic)
instance Field LinkTitle where
	type FieldValue LinkTitle = LinkTitle_
	fieldName _ = "link_title"
	fieldLabel = LinkTitle
unLinkTitle_ :: LinkTitle_ -> Text
unLinkTitle_ (LinkTitle_ x) = x

data AdvancedData = AdvancedData
newtype AdvancedData_ = AdvancedData_ A.Value deriving (Show, Generic)
instance Field AdvancedData where
	type FieldValue AdvancedData = AdvancedData_
	fieldName _ = "advanced_data"
	fieldLabel = AdvancedData
unAdvancedData_ :: AdvancedData_ -> A.Value
unAdvancedData_ (AdvancedData_ x) = x

data CallToActionType = CallToActionType
newtype CallToActionType_ = CallToActionType_ CallActionType deriving (Show, Generic)
instance Field CallToActionType where
	type FieldValue CallToActionType = CallToActionType_
	fieldName _ = "call_to_action_type"
	fieldLabel = CallToActionType
unCallToActionType_ :: CallToActionType_ -> CallActionType
unCallToActionType_ (CallToActionType_ x) = x

data InstagramPermalinkUrl = InstagramPermalinkUrl
newtype InstagramPermalinkUrl_ = InstagramPermalinkUrl_ Text deriving (Show, Generic)
instance Field InstagramPermalinkUrl where
	type FieldValue InstagramPermalinkUrl = InstagramPermalinkUrl_
	fieldName _ = "instagram_permalink_url"
	fieldLabel = InstagramPermalinkUrl
unInstagramPermalinkUrl_ :: InstagramPermalinkUrl_ -> Text
unInstagramPermalinkUrl_ (InstagramPermalinkUrl_ x) = x
instance A.FromJSON ActionSpec_
instance A.ToJSON ActionSpec_
instance A.FromJSON CallToAction_
instance A.ToJSON CallToAction_
instance A.FromJSON PlacePageSetId_
instance A.ToJSON PlacePageSetId_
instance A.FromJSON Value_
instance A.ToJSON Value_
instance A.FromJSON ImageFile_
instance A.ToJSON ImageFile_
instance A.FromJSON FollowRedirect_
instance A.ToJSON FollowRedirect_
instance A.FromJSON ObjectInstagramId_
instance A.ToJSON ObjectInstagramId_
instance A.FromJSON VideoId_
instance A.ToJSON VideoId_
instance A.FromJSON Image_
instance A.ToJSON Image_
instance A.FromJSON AppLink_
instance A.ToJSON AppLink_
instance A.FromJSON LinkCaption_
instance A.ToJSON LinkCaption_
instance A.FromJSON GetMovieShowtimes_
instance A.ToJSON GetMovieShowtimes_
instance A.FromJSON Leadgen_
instance A.ToJSON Leadgen_
instance A.FromJSON LinkDescription_
instance A.ToJSON LinkDescription_
instance A.FromJSON Sponsorship_
instance A.ToJSON Sponsorship_
instance A.FromJSON Application_
instance A.ToJSON Application_
instance A.FromJSON VideoAnnotation_
instance A.ToJSON VideoAnnotation_
instance A.FromJSON StartTimeInSec_
instance A.ToJSON StartTimeInSec_
instance A.FromJSON PolicyUrl_
instance A.ToJSON PolicyUrl_
instance A.FromJSON Link_
instance A.ToJSON Link_
instance A.FromJSON ProductLink_
instance A.ToJSON ProductLink_
instance A.FromJSON EndTimeInSec_
instance A.ToJSON EndTimeInSec_
instance A.FromJSON Page_
instance A.ToJSON Page_
instance A.FromJSON FallbackTestUrl_
instance A.ToJSON FallbackTestUrl_
instance A.FromJSON FollowUpActionUrl_
instance A.ToJSON FollowUpActionUrl_
instance A.FromJSON SplitFlowUsePost_
instance A.ToJSON SplitFlowUsePost_
instance A.FromJSON FollowUpTitle_
instance A.ToJSON FollowUpTitle_
instance A.FromJSON NeedSplitFlow_
instance A.ToJSON NeedSplitFlow_
instance A.FromJSON TcpaCompliant_
instance A.ToJSON TcpaCompliant_
instance A.FromJSON FollowUpActionText_
instance A.ToJSON FollowUpActionText_
instance A.FromJSON LandingPageCta_
instance A.ToJSON LandingPageCta_
instance A.FromJSON LinkTitle_
instance A.ToJSON LinkTitle_
instance A.FromJSON AdvancedData_
instance A.ToJSON AdvancedData_
instance A.FromJSON CallToActionType_
instance A.ToJSON CallToActionType_
instance A.FromJSON InstagramPermalinkUrl_
instance A.ToJSON InstagramPermalinkUrl_

instance ToBS ActionSpec_ where
	toBS (ActionSpec_ a) = toBS a

instance ToBS CallToAction_ where
	toBS (CallToAction_ a) = toBS a

instance ToBS PlacePageSetId_ where
	toBS (PlacePageSetId_ a) = toBS a

instance ToBS Value_ where
	toBS (Value_ a) = toBS a

instance ToBS ImageFile_ where
	toBS (ImageFile_ a) = toBS a

instance ToBS FollowRedirect_ where
	toBS (FollowRedirect_ a) = toBS a

instance ToBS ObjectInstagramId_ where
	toBS (ObjectInstagramId_ a) = toBS a

instance ToBS VideoId_ where
	toBS (VideoId_ a) = toBS a

instance ToBS Image_ where
	toBS (Image_ a) = toBS a

instance ToBS AppLink_ where
	toBS (AppLink_ a) = toBS a

instance ToBS LinkCaption_ where
	toBS (LinkCaption_ a) = toBS a

instance ToBS GetMovieShowtimes_ where
	toBS (GetMovieShowtimes_ a) = toBS a

instance ToBS Leadgen_ where
	toBS (Leadgen_ a) = toBS a

instance ToBS LinkDescription_ where
	toBS (LinkDescription_ a) = toBS a

instance ToBS Sponsorship_ where
	toBS (Sponsorship_ a) = toBS a

instance ToBS Application_ where
	toBS (Application_ a) = toBS a

instance ToBS VideoAnnotation_ where
	toBS (VideoAnnotation_ a) = toBS a

instance ToBS StartTimeInSec_ where
	toBS (StartTimeInSec_ a) = toBS a

instance ToBS PolicyUrl_ where
	toBS (PolicyUrl_ a) = toBS a

instance ToBS Link_ where
	toBS (Link_ a) = toBS a

instance ToBS ProductLink_ where
	toBS (ProductLink_ a) = toBS a

instance ToBS EndTimeInSec_ where
	toBS (EndTimeInSec_ a) = toBS a

instance ToBS Page_ where
	toBS (Page_ a) = toBS a

instance ToBS FallbackTestUrl_ where
	toBS (FallbackTestUrl_ a) = toBS a

instance ToBS FollowUpActionUrl_ where
	toBS (FollowUpActionUrl_ a) = toBS a

instance ToBS SplitFlowUsePost_ where
	toBS (SplitFlowUsePost_ a) = toBS a

instance ToBS FollowUpTitle_ where
	toBS (FollowUpTitle_ a) = toBS a

instance ToBS NeedSplitFlow_ where
	toBS (NeedSplitFlow_ a) = toBS a

instance ToBS TcpaCompliant_ where
	toBS (TcpaCompliant_ a) = toBS a

instance ToBS FollowUpActionText_ where
	toBS (FollowUpActionText_ a) = toBS a

instance ToBS LandingPageCta_ where
	toBS (LandingPageCta_ a) = toBS a

instance ToBS LinkTitle_ where
	toBS (LinkTitle_ a) = toBS a

instance ToBS AdvancedData_ where
	toBS (AdvancedData_ a) = toBS a

instance ToBS CallToActionType_ where
	toBS (CallToActionType_ a) = toBS a

instance ToBS InstagramPermalinkUrl_ where
	toBS (InstagramPermalinkUrl_ a) = toBS a

action_spec r = r `Rec.get` ActionSpec
call_to_action r = r `Rec.get` CallToAction
place_page_set_id r = r `Rec.get` PlacePageSetId
value r = r `Rec.get` Value
image_file r = r `Rec.get` ImageFile
follow_redirect r = r `Rec.get` FollowRedirect
object_instagram_id r = r `Rec.get` ObjectInstagramId
video_id r = r `Rec.get` VideoId
image r = r `Rec.get` Image
app_link r = r `Rec.get` AppLink
link_caption r = r `Rec.get` LinkCaption
get_movie_showtimes r = r `Rec.get` GetMovieShowtimes
leadgen r = r `Rec.get` Leadgen
link_description r = r `Rec.get` LinkDescription
sponsorship r = r `Rec.get` Sponsorship
application r = r `Rec.get` Application
video_annotation r = r `Rec.get` VideoAnnotation
start_time_in_sec r = r `Rec.get` StartTimeInSec
policy_url r = r `Rec.get` PolicyUrl
link r = r `Rec.get` Link
product_link r = r `Rec.get` ProductLink
end_time_in_sec r = r `Rec.get` EndTimeInSec
page r = r `Rec.get` Page
fallback_test_url r = r `Rec.get` FallbackTestUrl
follow_up_action_url r = r `Rec.get` FollowUpActionUrl
split_flow_use_post r = r `Rec.get` SplitFlowUsePost
follow_up_title r = r `Rec.get` FollowUpTitle
need_split_flow r = r `Rec.get` NeedSplitFlow
tcpa_compliant r = r `Rec.get` TcpaCompliant
follow_up_action_text r = r `Rec.get` FollowUpActionText
landing_page_cta r = r `Rec.get` LandingPageCta
link_title r = r `Rec.get` LinkTitle
advanced_data r = r `Rec.get` AdvancedData
call_to_action_type r = r `Rec.get` CallToActionType
instagram_permalink_url r = r `Rec.get` InstagramPermalinkUrl
-- Entity:AdCreative, mode:Reading
class IsAdCreativeGetField r
instance (IsAdCreativeGetField h, IsAdCreativeGetField t) => IsAdCreativeGetField (h :*: t)
instance IsAdCreativeGetField Nil
instance IsAdCreativeGetField ActorId
instance IsAdCreativeGetField ActorImageHash
instance IsAdCreativeGetField CallToActionType
instance IsAdCreativeGetField InstagramPermalinkUrl
instance IsAdCreativeGetField ImageHash
instance IsAdCreativeGetField Name
instance IsAdCreativeGetField ObjectStoryId
instance IsAdCreativeGetField ObjectUrl
instance IsAdCreativeGetField LinkUrl
instance IsAdCreativeGetField InstagramActorId
instance IsAdCreativeGetField ActorImageUrl
instance IsAdCreativeGetField RunStatus
instance IsAdCreativeGetField ProductSetId
instance IsAdCreativeGetField TemplateUrl
instance IsAdCreativeGetField ActorName
instance IsAdCreativeGetField UrlTags
instance IsAdCreativeGetField ThumbnailUrl
instance IsAdCreativeGetField ImageUrl
instance IsAdCreativeGetField Title
instance IsAdCreativeGetField LinkOgId
instance IsAdCreativeGetField ObjectId
instance IsAdCreativeGetField Body
instance IsAdCreativeGetField Id

type AdCreativeGet fl r = (A.FromJSON r, IsAdCreativeGetField r, FieldListToRec fl r)
type AdCreativeGetRet r = r -- Default fields
getAdCreative :: (R.MonadResource m, MonadBaseControl IO m, AdCreativeGet fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (AdCreativeGetRet r)
getAdCreative (Id_ id) fl mtoken = getObject ("/v2.5/" <> id <> "/adcreatives") [("fields", textListToBS $ fieldNameList $ fl)] mtoken


-- Entity:AdCreative, mode:Creating
class IsAdCreativeSetField r
instance (IsAdCreativeSetField h, IsAdCreativeSetField t) => IsAdCreativeSetField (h :*: t)
instance IsAdCreativeSetField Nil
instance IsAdCreativeSetField ActorImageUrl
instance IsAdCreativeSetField ActorImageHash
instance IsAdCreativeSetField ActionSpec
instance IsAdCreativeSetField LinkOgId
instance IsAdCreativeSetField CallToAction
instance IsAdCreativeSetField PlacePageSetId
instance IsAdCreativeSetField ActorName
instance IsAdCreativeSetField Value
instance IsAdCreativeSetField ObjectId
instance IsAdCreativeSetField ImageFile
instance IsAdCreativeSetField ProductSetId
instance IsAdCreativeSetField InstagramActorId
instance IsAdCreativeSetField FollowRedirect
instance IsAdCreativeSetField ActorId
instance IsAdCreativeSetField ThumbnailUrl
instance IsAdCreativeSetField TemplateUrl
instance IsAdCreativeSetField LinkUrl
instance IsAdCreativeSetField ObjectStoryId
instance IsAdCreativeSetField UrlTags
instance IsAdCreativeSetField Name
instance IsAdCreativeSetField ImageHash
instance IsAdCreativeSetField ObjectInstagramId
instance IsAdCreativeSetField VideoId
instance IsAdCreativeSetField Title
instance IsAdCreativeSetField ObjectUrl
instance IsAdCreativeSetField Image
instance IsAdCreativeSetField AppLink
instance IsAdCreativeSetField LinkCaption
instance IsAdCreativeSetField GetMovieShowtimes
instance IsAdCreativeSetField Leadgen
instance IsAdCreativeSetField LinkDescription
instance IsAdCreativeSetField Sponsorship
instance IsAdCreativeSetField Application
instance IsAdCreativeSetField VideoAnnotation
instance IsAdCreativeSetField StartTimeInSec
instance IsAdCreativeSetField Body
instance IsAdCreativeSetField PolicyUrl
instance IsAdCreativeSetField Link
instance IsAdCreativeSetField ProductLink
instance IsAdCreativeSetField EndTimeInSec
instance IsAdCreativeSetField Page
instance IsAdCreativeSetField FallbackTestUrl
instance IsAdCreativeSetField FollowUpActionUrl
instance IsAdCreativeSetField SplitFlowUsePost
instance IsAdCreativeSetField FollowUpTitle
instance IsAdCreativeSetField NeedSplitFlow
instance IsAdCreativeSetField TcpaCompliant
instance IsAdCreativeSetField ImageUrl
instance IsAdCreativeSetField OfferId
instance IsAdCreativeSetField FollowUpActionText
instance IsAdCreativeSetField LandingPageCta
instance IsAdCreativeSetField LinkTitle
instance IsAdCreativeSetField Id
instance IsAdCreativeSetField AdvancedData

type AdCreativeSet r = (Has Id r, A.FromJSON r, IsAdCreativeSetField r, ToForm r)
setAdCreative :: (R.MonadResource m, MonadBaseControl IO m, AdCreativeSet r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
setAdCreative (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/adcreatives") (toForm r) mtoken


-- Entity:AdCreative, mode:Updating
class IsAdCreativeUpdField r
instance (IsAdCreativeUpdField h, IsAdCreativeUpdField t) => IsAdCreativeUpdField (h :*: t)
instance IsAdCreativeUpdField Nil
instance IsAdCreativeUpdField Name
instance IsAdCreativeUpdField RunStatus
instance IsAdCreativeUpdField AccountId
instance IsAdCreativeUpdField Id

type AdCreativeUpd r = (Has Id r, A.FromJSON r, IsAdCreativeUpdField r, ToForm r)
updAdCreative :: (R.MonadResource m, MonadBaseControl IO m, AdCreativeUpd r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
updAdCreative (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/adcreatives") (toForm r) mtoken


-- Entity:AdCreative, mode:Deleting
class IsAdCreativeDelField r
instance (IsAdCreativeDelField h, IsAdCreativeDelField t) => IsAdCreativeDelField (h :*: t)
instance IsAdCreativeDelField Nil
instance IsAdCreativeDelField RunStatus
instance IsAdCreativeDelField Name
instance IsAdCreativeDelField Id
instance IsAdCreativeDelField AccountId

type AdCreativeDel r = (Has Id r, A.FromJSON r, IsAdCreativeDelField r, ToForm r)
delAdCreative :: (R.MonadResource m, MonadBaseControl IO m, AdCreativeDel r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
delAdCreative (Id_ id) r mtoken = deleteForm ("/v2.5/" <> id <> "/adcreatives") (toForm r) mtoken

