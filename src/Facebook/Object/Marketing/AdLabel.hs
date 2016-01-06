{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Facebook.Object.Marketing.AdLabel where

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

-- Entity:AdLabel, mode:Reading
class IsAdLabelGetField r
instance (IsAdLabelGetField h, IsAdLabelGetField t) => IsAdLabelGetField (h :*: t)
instance IsAdLabelGetField Nil
instance IsAdLabelGetField Name
instance IsAdLabelGetField Id
instance IsAdLabelGetField CreatedTime
instance IsAdLabelGetField UpdatedTime

type AdLabelGet fl r = (A.FromJSON r, IsAdLabelGetField r, FieldListToRec fl r)
type AdLabelGetRet r = Name :*: Id :*: r -- Default fields
getAdLabel :: (R.MonadResource m, MonadBaseControl IO m, AdLabelGet fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (AdLabelGetRet r)
getAdLabel (Id_ id) fl mtoken = getObject ("/v2.5/" <> id <> "") [("fields", textListToBS $ fieldNameList $ Name ::: Id ::: fl)] mtoken


-- Entity:AdLabel, mode:Creating
class IsAdLabelSetField r
instance (IsAdLabelSetField h, IsAdLabelSetField t) => IsAdLabelSetField (h :*: t)
instance IsAdLabelSetField Nil
instance IsAdLabelSetField Id
instance IsAdLabelSetField Name

type AdLabelSet r = (Has Id r, Has Name r, A.FromJSON r, IsAdLabelSetField r, ToForm r)
setAdLabel :: (R.MonadResource m, MonadBaseControl IO m, AdLabelSet r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
setAdLabel (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "") (toForm r) mtoken


-- Entity:AdLabel, mode:Updating
class IsAdLabelUpdField r
instance (IsAdLabelUpdField h, IsAdLabelUpdField t) => IsAdLabelUpdField (h :*: t)
instance IsAdLabelUpdField Nil
instance IsAdLabelUpdField ExecutionOptions
instance IsAdLabelUpdField Id
instance IsAdLabelUpdField Id
instance IsAdLabelUpdField ExecutionOptions
instance IsAdLabelUpdField ExecutionOptions
instance IsAdLabelUpdField Id
instance IsAdLabelUpdField Id
instance IsAdLabelUpdField Id
instance IsAdLabelUpdField Name

type AdLabelUpd r = (Has Id r, Has Id r, Has Id r, Has Id r, Has Id r, Has Name r, A.FromJSON r, IsAdLabelUpdField r, ToForm r)
updAdLabel :: (R.MonadResource m, MonadBaseControl IO m, AdLabelUpd r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
updAdLabel (Id_ id) r mtoken = postForm ("/v2.5/" <> id <> "") (toForm r) mtoken


-- Entity:AdLabel, mode:Deleting
class IsAdLabelDelField r
instance (IsAdLabelDelField h, IsAdLabelDelField t) => IsAdLabelDelField (h :*: t)
instance IsAdLabelDelField Nil
instance IsAdLabelDelField ExecutionOptions
instance IsAdLabelDelField ExecutionOptions
instance IsAdLabelDelField Id
instance IsAdLabelDelField ExecutionOptions
instance IsAdLabelDelField Id
instance IsAdLabelDelField Id
instance IsAdLabelDelField Id
instance IsAdLabelDelField Id

type AdLabelDel r = (Has Id r, Has Id r, Has Id r, Has Id r, Has Id r, A.FromJSON r, IsAdLabelDelField r, ToForm r)
delAdLabel :: (R.MonadResource m, MonadBaseControl IO m, AdLabelDel r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	-> Maybe UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m r
delAdLabel (Id_ id) r mtoken = deleteForm ("/v2.5/" <> id <> "") (toForm r) mtoken

