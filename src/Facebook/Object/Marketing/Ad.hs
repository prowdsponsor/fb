{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.Ad where

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

data Status = Status
newtype Status_ = Status_ ConfigureStatusADT deriving (Show, Generic)
instance A.FromJSON Status_
instance A.ToJSON Status_
instance Field Status where
	type FieldValue Status = Status_
	fieldName _ = "status"
	fieldLabel = Status

data DateFormat = DateFormat
newtype DateFormat_ = DateFormat_ Text deriving (Show, Generic)
instance A.FromJSON DateFormat_
instance A.ToJSON DateFormat_
instance Field DateFormat where
	type FieldValue DateFormat = DateFormat_
	fieldName _ = "date_format"
	fieldLabel = DateFormat

data CreativeId = CreativeId
newtype CreativeId_ = CreativeId_ Text deriving (Show, Generic)
instance A.FromJSON CreativeId_
instance A.ToJSON CreativeId_
instance Field CreativeId where
	type FieldValue CreativeId = CreativeId_
	fieldName _ = "creative_id"
	fieldLabel = CreativeId

data BidType = BidType
newtype BidType_ = BidType_ BidTypeADT deriving (Show, Generic)
instance A.FromJSON BidType_
instance A.ToJSON BidType_
instance Field BidType where
	type FieldValue BidType = BidType_
	fieldName _ = "bid_type"
	fieldLabel = BidType

data LastUpdatedByAppId = LastUpdatedByAppId
newtype LastUpdatedByAppId_ = LastUpdatedByAppId_ Text deriving (Show, Generic)
instance A.FromJSON LastUpdatedByAppId_
instance A.ToJSON LastUpdatedByAppId_
instance Field LastUpdatedByAppId where
	type FieldValue LastUpdatedByAppId = LastUpdatedByAppId_
	fieldName _ = "last_updated_by_app_id"
	fieldLabel = LastUpdatedByAppId

instance ToBS Status_ where
	toBS (Status_ a) = toBS a

instance ToBS DateFormat_ where
	toBS (DateFormat_ a) = toBS a

instance ToBS CreativeId_ where
	toBS (CreativeId_ a) = toBS a

instance ToBS BidType_ where
	toBS (BidType_ a) = toBS a

instance ToBS LastUpdatedByAppId_ where
	toBS (LastUpdatedByAppId_ a) = toBS a

status r = r `Rec.get` Status
date_format r = r `Rec.get` DateFormat
creative_id r = r `Rec.get` CreativeId
bid_type r = r `Rec.get` BidType
last_updated_by_app_id r = r `Rec.get` LastUpdatedByAppId
-- Entity:Ad, mode:Reading
class IsAdGetField r
instance (IsAdGetField h, IsAdGetField t) => IsAdGetField (h :*: t)
instance IsAdGetField Nil
instance IsAdGetField EffectiveStatus
instance IsAdGetField CreativeId
instance IsAdGetField AccountId
instance IsAdGetField AdsetId
instance IsAdGetField CampaignId
instance IsAdGetField ConfiguredStatus
instance IsAdGetField BidType
instance IsAdGetField Id
instance IsAdGetField LastUpdatedByAppId
instance IsAdGetField BidAmount
instance IsAdGetField UpdatedTime
instance IsAdGetField Name
instance IsAdGetField CreatedTime

type AdGet fl r = (A.FromJSON r, IsAdGetField r, FieldListToRec fl r)
type AdGetRet r = CreativeId :*: Id :*: r -- Default fields
getAd :: (R.MonadResource m, MonadBaseControl IO m, AdGet fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (AdGetRet r)
getAd (Id_ id) fl mtoken = getObject ("/v2.5/" <> id <> "/ads") [("fields", textListToBS $ fieldNameList $ CreativeId ::: Id ::: fl)] mtoken


-- Entity:Ad, mode:Creating
class IsAdSetField r
instance (IsAdSetField h, IsAdSetField t) => IsAdSetField (h :*: t)
instance IsAdSetField Nil
instance IsAdSetField CampaignGroupId
instance IsAdSetField ExecutionOptions
instance IsAdSetField DisplaySequence
instance IsAdSetField Id
instance IsAdSetField Redownload
instance IsAdSetField Creative
instance IsAdSetField Name
instance IsAdSetField Status
instance IsAdSetField BidAmount
instance IsAdSetField DateFormat
instance IsAdSetField AdsetId

type AdSet r = (Has Id r, Has Creative r, A.FromJSON r, IsAdSetField r, ToForm r)
setAd :: (R.MonadResource m, MonadBaseControl IO m, AdSet r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
setAd (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/ads") (toForm r) mtoken


-- Entity:Ad, mode:Updating
class IsAdUpdField r
instance (IsAdUpdField h, IsAdUpdField t) => IsAdUpdField (h :*: t)
instance IsAdUpdField Nil
instance IsAdUpdField ExecutionOptions
instance IsAdUpdField DisplaySequence
instance IsAdUpdField CampaignGroupId
instance IsAdUpdField Redownload
instance IsAdUpdField Status
instance IsAdUpdField Id
instance IsAdUpdField Creative
instance IsAdUpdField BidAmount
instance IsAdUpdField AdsetId
instance IsAdUpdField Name

type AdUpd r = (Has Id r, A.FromJSON r, IsAdUpdField r, ToForm r)
updAd :: (R.MonadResource m, MonadBaseControl IO m, AdUpd r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
updAd (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/ads") (toForm r) mtoken


-- Entity:Ad, mode:Deleting
class IsAdDelField r
instance (IsAdDelField h, IsAdDelField t) => IsAdDelField (h :*: t)
instance IsAdDelField Nil
instance IsAdDelField Id

type AdDel r = (Has Id r, A.FromJSON r, IsAdDelField r, ToForm r)
delAd :: (R.MonadResource m, MonadBaseControl IO m, AdDel r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
delAd (Id_ id) r mtoken = deleteForm ("/v2.5/" <> id <> "/ads") (toForm r) mtoken

