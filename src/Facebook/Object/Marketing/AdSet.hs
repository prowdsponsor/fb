{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.AdSet where

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

data BudgetRemaining = BudgetRemaining
newtype BudgetRemaining_ = BudgetRemaining_ Text deriving (Show, Generic)
instance A.FromJSON BudgetRemaining_
instance A.ToJSON BudgetRemaining_
instance Field BudgetRemaining where
	type FieldValue BudgetRemaining = BudgetRemaining_
	fieldName _ = "budget_remaining"
	fieldLabel = BudgetRemaining

data FrequencyCapResetPeriod = FrequencyCapResetPeriod
newtype FrequencyCapResetPeriod_ = FrequencyCapResetPeriod_ Word32 deriving (Show, Generic)
instance A.FromJSON FrequencyCapResetPeriod_
instance A.ToJSON FrequencyCapResetPeriod_
instance Field FrequencyCapResetPeriod where
	type FieldValue FrequencyCapResetPeriod = FrequencyCapResetPeriod_
	fieldName _ = "frequency_cap_reset_period"
	fieldLabel = FrequencyCapResetPeriod

data FrequencyCap = FrequencyCap
newtype FrequencyCap_ = FrequencyCap_ Word32 deriving (Show, Generic)
instance A.FromJSON FrequencyCap_
instance A.ToJSON FrequencyCap_
instance Field FrequencyCap where
	type FieldValue FrequencyCap = FrequencyCap_
	fieldName _ = "frequency_cap"
	fieldLabel = FrequencyCap

instance ToBS Status_ where
	toBS (Status_ a) = toBS a

instance ToBS BudgetRemaining_ where
	toBS (BudgetRemaining_ a) = toBS a

instance ToBS FrequencyCapResetPeriod_ where
	toBS (FrequencyCapResetPeriod_ a) = toBS a

instance ToBS FrequencyCap_ where
	toBS (FrequencyCap_ a) = toBS a

status r = r `Rec.get` Status
budget_remaining r = r `Rec.get` BudgetRemaining
frequency_cap_reset_period r = r `Rec.get` FrequencyCapResetPeriod
frequency_cap r = r `Rec.get` FrequencyCap
-- Entity:AdSet, mode:Reading
class IsAdSetGetField r
instance (IsAdSetGetField h, IsAdSetGetField t) => IsAdSetGetField (h :*: t)
instance IsAdSetGetField Nil
instance IsAdSetGetField BidAmount
instance IsAdSetGetField LifetimeImps
instance IsAdSetGetField BudgetRemaining
instance IsAdSetGetField DailyBudget
instance IsAdSetGetField FrequencyCapResetPeriod
instance IsAdSetGetField StartTime
instance IsAdSetGetField ConfiguredStatus
instance IsAdSetGetField IsAutobid
instance IsAdSetGetField LifetimeFrequencyCap
instance IsAdSetGetField LifetimeBudget
instance IsAdSetGetField EffectiveStatus
instance IsAdSetGetField EndTime
instance IsAdSetGetField RtbFlag
instance IsAdSetGetField CampaignId
instance IsAdSetGetField Name
instance IsAdSetGetField FrequencyCap
instance IsAdSetGetField CreatedTime
instance IsAdSetGetField UpdatedTime
instance IsAdSetGetField Id
instance IsAdSetGetField AccountId

type AdSetGet fl r = (A.FromJSON r, IsAdSetGetField r, FieldListToRec fl r)
type AdSetGetRet r = Id :*: r -- Default fields
getAdSet :: (R.MonadResource m, MonadBaseControl IO m, AdSetGet fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (Pager (AdSetGetRet r))
getAdSet (Id_ id) fl mtoken = getObject ("/v2.5/" <> id <> "/adsets") [("fields", textListToBS $ fieldNameList $ Id ::: fl)] $ Just mtoken


-- Entity:AdSet, mode:Creating
class IsAdSetSetField r
instance (IsAdSetSetField h, IsAdSetSetField t) => IsAdSetSetField (h :*: t)
instance IsAdSetSetField Nil
instance IsAdSetSetField ExecutionOptions
instance IsAdSetSetField BidAmount
instance IsAdSetSetField IsAutobid
instance IsAdSetSetField StartMinute
instance IsAdSetSetField CampaignId
instance IsAdSetSetField Name
instance IsAdSetSetField EndMinute
instance IsAdSetSetField EndTime
instance IsAdSetSetField CreativeSequence
instance IsAdSetSetField LifetimeBudget
instance IsAdSetSetField OfferId
instance IsAdSetSetField StartTime
instance IsAdSetSetField Status
instance IsAdSetSetField LifetimeFrequencyCap
instance IsAdSetSetField Redownload
instance IsAdSetSetField ApplicationId
instance IsAdSetSetField PixelId
instance IsAdSetSetField RtbFlag
instance IsAdSetSetField Days
instance IsAdSetSetField ObjectStoreUrl
instance IsAdSetSetField DailyBudget
instance IsAdSetSetField DailyImps
instance IsAdSetSetField ProductCatalogId
instance IsAdSetSetField LifetimeImps
instance IsAdSetSetField ProductSetId
instance IsAdSetSetField TimeStart
instance IsAdSetSetField TimeStop
instance IsAdSetSetField PageId

type AdSetSet r = (A.FromJSON r, IsAdSetSetField r, ToForm r)
setAdSet :: (R.MonadResource m, MonadBaseControl IO m, AdSetSet r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
setAdSet (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/adsets") (toForm r) mtoken


-- Entity:AdSet, mode:Updating
class IsAdSetUpdField r
instance (IsAdSetUpdField h, IsAdSetUpdField t) => IsAdSetUpdField (h :*: t)
instance IsAdSetUpdField Nil
instance IsAdSetUpdField ExecutionOptions
instance IsAdSetUpdField BidAmount
instance IsAdSetUpdField DailyImps
instance IsAdSetUpdField EndMinute
instance IsAdSetUpdField ApplicationId
instance IsAdSetUpdField IsAutobid
instance IsAdSetUpdField StartMinute
instance IsAdSetUpdField Days
instance IsAdSetUpdField PixelId
instance IsAdSetUpdField EndTime
instance IsAdSetUpdField Name
instance IsAdSetUpdField LifetimeImps
instance IsAdSetUpdField DailyBudget
instance IsAdSetUpdField OfferId
instance IsAdSetUpdField LifetimeBudget
instance IsAdSetUpdField Redownload
instance IsAdSetUpdField ObjectStoreUrl
instance IsAdSetUpdField ProductSetId
instance IsAdSetUpdField LifetimeFrequencyCap
instance IsAdSetUpdField Status
instance IsAdSetUpdField ProductCatalogId
instance IsAdSetUpdField StartTime
instance IsAdSetUpdField CreativeSequence
instance IsAdSetUpdField TimeStop
instance IsAdSetUpdField AccountId
instance IsAdSetUpdField TimeStart
instance IsAdSetUpdField PageId
instance IsAdSetUpdField Id

type AdSetUpd r = (A.FromJSON r, IsAdSetUpdField r, ToForm r)
updAdSet :: (R.MonadResource m, MonadBaseControl IO m, AdSetUpd r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
updAdSet (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "/adsets") (toForm r) mtoken


-- Entity:AdSet, mode:Deleting
class IsAdSetDelField r
instance (IsAdSetDelField h, IsAdSetDelField t) => IsAdSetDelField (h :*: t)
instance IsAdSetDelField Nil
instance IsAdSetDelField Id
instance IsAdSetDelField AccountId

type AdSetDel r = (A.FromJSON r, IsAdSetDelField r, ToForm r)
delAdSet :: (R.MonadResource m, MonadBaseControl IO m, AdSetDel r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
delAdSet (Id_ id) r mtoken = deleteForm ("/v2.5/" <> id <> "/adsets") (toForm r) mtoken

