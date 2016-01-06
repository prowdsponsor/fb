{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.AdCampaign where

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

data ObjectCount = ObjectCount
newtype ObjectCount_ = ObjectCount_ Integer deriving (Show, Generic)
instance A.FromJSON ObjectCount_
instance A.ToJSON ObjectCount_
instance Field ObjectCount where
	type FieldValue ObjectCount = ObjectCount_
	fieldName _ = "object_count"
	fieldLabel = ObjectCount

data BeforeDate = BeforeDate
newtype BeforeDate_ = BeforeDate_ UTCTime deriving Generic
instance A.FromJSON BeforeDate_
instance A.ToJSON BeforeDate_
instance Field BeforeDate where
	type FieldValue BeforeDate = BeforeDate_
	fieldName _ = "before_date"
	fieldLabel = BeforeDate

data Status = Status
newtype Status_ = Status_ ConfigureStatusADT deriving (Show, Generic)
instance A.FromJSON Status_
instance A.ToJSON Status_
instance Field Status where
	type FieldValue Status = Status_
	fieldName _ = "status"
	fieldLabel = Status

data CanUseSpendCap = CanUseSpendCap
newtype CanUseSpendCap_ = CanUseSpendCap_ Bool deriving (Show, Generic)
instance A.FromJSON CanUseSpendCap_
instance A.ToJSON CanUseSpendCap_
instance Field CanUseSpendCap where
	type FieldValue CanUseSpendCap = CanUseSpendCap_
	fieldName _ = "can_use_spend_cap"
	fieldLabel = CanUseSpendCap

data StopTime = StopTime
newtype StopTime_ = StopTime_ UTCTime deriving Generic
instance A.FromJSON StopTime_
instance A.ToJSON StopTime_
instance Field StopTime where
	type FieldValue StopTime = StopTime_
	fieldName _ = "stop_time"
	fieldLabel = StopTime

instance ToBS ObjectCount_ where
	toBS (ObjectCount_ a) = toBS a

instance ToBS BeforeDate_ where
	toBS (BeforeDate_ a) = toBS a

instance ToBS Status_ where
	toBS (Status_ a) = toBS a

instance ToBS CanUseSpendCap_ where
	toBS (CanUseSpendCap_ a) = toBS a

instance ToBS StopTime_ where
	toBS (StopTime_ a) = toBS a

object_count r = r `Rec.get` ObjectCount
before_date r = r `Rec.get` BeforeDate
status r = r `Rec.get` Status
can_use_spend_cap r = r `Rec.get` CanUseSpendCap
stop_time r = r `Rec.get` StopTime
-- Entity:AdCampaign, mode:Reading
class IsAdCampaignGetField r
instance (IsAdCampaignGetField h, IsAdCampaignGetField t) => IsAdCampaignGetField (h :*: t)
instance IsAdCampaignGetField Nil
instance IsAdCampaignGetField SpendCap
instance IsAdCampaignGetField AccountId
instance IsAdCampaignGetField ConfiguredStatus
instance IsAdCampaignGetField BuyingType
instance IsAdCampaignGetField Objective
instance IsAdCampaignGetField CanUseSpendCap
instance IsAdCampaignGetField Id
instance IsAdCampaignGetField StopTime
instance IsAdCampaignGetField Name
instance IsAdCampaignGetField EffectiveStatus
instance IsAdCampaignGetField StartTime
instance IsAdCampaignGetField UpdatedTime
instance IsAdCampaignGetField CreatedTime

type AdCampaignGet fl r = (A.FromJSON r, IsAdCampaignGetField r, FieldListToRec fl r)
type AdCampaignGetRet r = Id :*: r -- Default fields
getAdCampaign :: (R.MonadResource m, MonadBaseControl IO m, AdCampaignGet fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (Pager (AdCampaignGetRet r))
getAdCampaign (Id_ id) fl mtoken = getObject ("/v2.5/" <> id <> "/campaigns") [("fields", textListToBS $ fieldNameList $ Id ::: fl)] $ Just mtoken


-- Entity:AdCampaign, mode:Creating
class IsAdCampaignSetField r
instance (IsAdCampaignSetField h, IsAdCampaignSetField t) => IsAdCampaignSetField (h :*: t)
instance IsAdCampaignSetField Nil
instance IsAdCampaignSetField ExecutionOptions
instance IsAdCampaignSetField ApplicationId
instance IsAdCampaignSetField Status
instance IsAdCampaignSetField BuyingType
instance IsAdCampaignSetField ObjectStoreUrl
instance IsAdCampaignSetField ProductSetId
instance IsAdCampaignSetField ProductCatalogId
instance IsAdCampaignSetField OfferId
instance IsAdCampaignSetField PixelId
instance IsAdCampaignSetField SpendCap
instance IsAdCampaignSetField Name
instance IsAdCampaignSetField PageId
instance IsAdCampaignSetField Objective

type AdCampaignSet r = (A.FromJSON r, IsAdCampaignSetField r, ToForm r)
setAdCampaign :: (R.MonadResource m, MonadBaseControl IO m, AdCampaignSet r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m SuccessId
setAdCampaign (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/campaigns") (toForm r) mtoken


-- Entity:AdCampaign, mode:Updating
class IsAdCampaignUpdField r
instance (IsAdCampaignUpdField h, IsAdCampaignUpdField t) => IsAdCampaignUpdField (h :*: t)
instance IsAdCampaignUpdField Nil
instance IsAdCampaignUpdField ExecutionOptions
instance IsAdCampaignUpdField SpendCap
instance IsAdCampaignUpdField Status
instance IsAdCampaignUpdField Name
instance IsAdCampaignUpdField Objective

type AdCampaignUpd r = (A.FromJSON r, IsAdCampaignUpdField r, ToForm r)
updAdCampaign :: (R.MonadResource m, MonadBaseControl IO m, AdCampaignUpd r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
updAdCampaign (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/campaigns") (toForm r) mtoken


-- Entity:AdCampaign, mode:Deleting
class IsAdCampaignDelField r
instance (IsAdCampaignDelField h, IsAdCampaignDelField t) => IsAdCampaignDelField (h :*: t)
instance IsAdCampaignDelField Nil
instance IsAdCampaignDelField ObjectCount
instance IsAdCampaignDelField Id
instance IsAdCampaignDelField BeforeDate

type AdCampaignDel r = (Has Id r, A.FromJSON r, IsAdCampaignDelField r, ToForm r)
delAdCampaign :: (R.MonadResource m, MonadBaseControl IO m, AdCampaignDel r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
delAdCampaign (Id_ id) r mtoken = deleteForm ("/v2.5/" <> id <> "/campaigns") (toForm r) mtoken

