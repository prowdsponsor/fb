{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Facebook.RealTime
    ( RealTimeUpdateObject(..)
    , RealTimeUpdateField
    , RealTimeUpdateUrl
    , RealTimeUpdateToken
    , modifySubscription
    , RealTimeUpdateSubscription(..)
    , listSubscriptions
    , verifyRealTimeUpdateNotifications
    , getRealTimeUpdateNotifications
    , RealTimeUpdateNotification(..)
    , RealTimeUpdateNotificationUserEntry(..)
    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad (liftM, mzero, void)
import Crypto.Hash.CryptoAPI (SHA1)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)

import qualified Control.Monad.Trans.Resource as R
import qualified Crypto.Classes as Crypto
import qualified Crypto.HMAC as Crypto
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Facebook.Types
import Facebook.Monad
import Facebook.Base
import Facebook.Graph
import Facebook.Pager


-- | The type of objects that a real-time update refers to.
data RealTimeUpdateObject =
    UserRTUO
  | PermissionsRTUO
  | PageRTUO
  | ErrorsRTUO
  | OtherRTUO Text
    deriving (Eq, Ord, Show, Typeable)

rtuoToBS :: RealTimeUpdateObject -> ByteString
rtuoToBS (UserRTUO)        = "user"
rtuoToBS (PermissionsRTUO) = "permissions"
rtuoToBS (PageRTUO)        = "page"
rtuoToBS (ErrorsRTUO)      = "errors"
rtuoToBS (OtherRTUO other) = TE.encodeUtf8 other

instance A.FromJSON RealTimeUpdateObject where
  parseJSON (A.String "user")        = return UserRTUO
  parseJSON (A.String "permissions") = return PermissionsRTUO
  parseJSON (A.String "page")        = return PageRTUO
  parseJSON (A.String "errors")      = return ErrorsRTUO
  parseJSON (A.String other)         = return (OtherRTUO other)
  parseJSON _ = mzero

instance A.ToJSON RealTimeUpdateObject where
  toJSON = A.String . TE.decodeUtf8 . rtuoToBS


-- | A field of a 'RealTimeUpdateObject' that you would like to
-- receive notifications when changed.
type RealTimeUpdateField = ByteString


-- | The URL on your server that will receive the real-time
-- updates.  Please refer to Facebook's documentation in order to
-- see what this URL needs to implement.
type RealTimeUpdateUrl = Text


-- | A token that is sent back by Facebook's servers to your
-- server in order to verify that you really were trying to
-- modify your subscription.
type RealTimeUpdateToken = ByteString


-- | Add or modify a subscription for real-time updates.  If
-- there were no previous subscriptions for the given
-- 'RealTimeUpdateObject', then a new subscription is created.
-- If there was any previous subscription for the given
-- 'RealTimeUpdateObject', it's overriden by this one (even if
-- the other subscription had a different callback URL).
modifySubscription :: (R.MonadResource m, MonadBaseControl IO m) =>
                      RealTimeUpdateObject
                      -- ^ Type of objects whose subscription you
                      -- and to add or modify.
                   -> [RealTimeUpdateField]
                      -- ^ Fields that you are interested in
                      -- receiving updates.
                   -> RealTimeUpdateUrl
                      -- ^ Your callback URL.
                   -> RealTimeUpdateToken
                      -- ^ A verification token.
                   -> AppAccessToken
                      -- ^ Access token for your app.
                   -> FacebookT Auth m ()
modifySubscription object fields callbackUrl verifyToken apptoken = do
  path <- getSubscriptionsPath
  let args = [ "object"       #= rtuoToBS object
             , "fields"       #= fields
             , "callback_url" #= callbackUrl
             , "verify_token" #= verifyToken
             ]
  runResourceInFb $ do
    req <- fbreq path (Just apptoken) args
    void $ fbhttp req { H.method = HT.methodPost }
  return ()


-- | (Internal)  Get the subscription's path.
getSubscriptionsPath :: Monad m => FacebookT Auth m Text
getSubscriptionsPath = do
  creds <- getCreds
  return $ T.concat ["/", appId creds, "/subscriptions"]


-- | Information returned by Facebook about a real-time update
-- notification subscription.
data RealTimeUpdateSubscription =
  RealTimeUpdateSubscription {
    rtusObject      :: RealTimeUpdateObject
  , rtusCallbackUrl :: RealTimeUpdateUrl
  , rtusFields      :: [RealTimeUpdateField]
  , rtusActive      :: Bool
  } deriving (Eq, Ord, Show, Typeable)

instance A.FromJSON RealTimeUpdateSubscription where
  parseJSON (A.Object v) =
    RealTimeUpdateSubscription
      <$> v A..: "object"
      <*> v A..: "callback_url"
      <*> fmap (map encodeUtf8) (v A..: "fields")
      <*> v A..: "active"
  parseJSON _ = mzero


-- | List current real-time update subscriptions.
listSubscriptions ::
  (R.MonadResource m, MonadBaseControl IO m) =>
  AppAccessToken -> FacebookT Auth m [RealTimeUpdateSubscription]
listSubscriptions apptoken = do
  path <- getSubscriptionsPath
  pager <- getObject path [] (Just apptoken)
  src <- fetchAllNextPages pager
  lift $ src C.$$ CL.consume


-- | Verifies the input's authenticity (i.e. it comes from
-- Facebook) and integrity by calculating its HMAC-SHA1 (using
-- your application secret as the key) and verifying that it
-- matches the value from the HTTP request's @X-Hub-Signature@
-- header's value.  If it's not valid, @Nothing@ is returned,
-- otherwise @Just data@ is returned where @data@ is the original
-- data.
verifyRealTimeUpdateNotifications ::
     Monad m =>
     ByteString
     -- ^ @X-Hub-Signature@ HTTP header's value.
  -> L.ByteString
     -- ^ Request body with JSON-encoded notifications.
  -> FacebookT Auth m (Maybe L.ByteString)
verifyRealTimeUpdateNotifications sig body = do
  creds <- getCreds
  let key :: Crypto.MacKey ctx SHA1
      key = Crypto.MacKey (appSecretBS creds)
      hash = Crypto.hmac key body
      expected = "sha1=" <> Base16.encode (Crypto.encode hash)
  return $! if sig `Crypto.constTimeEq` expected then Just body else Nothing


-- | Same as 'verifyRealTimeUpdateNotifications' but also parses
-- the response as JSON.  Returns @Nothing@ if either the
-- signature is invalid or the data can't be parsed (use
-- 'verifyRealTimeUpdateNotifications' if you need to distinguish
-- between these two error conditions).
getRealTimeUpdateNotifications ::
     (Monad m, A.FromJSON a) =>
     ByteString
     -- ^ @X-Hub-Signature@ HTTP header's value.
  -> L.ByteString
     -- ^ Request body with JSON-encoded notifications.
  -> FacebookT Auth m (Maybe (RealTimeUpdateNotification a))
getRealTimeUpdateNotifications =
  (liftM (>>= A.decode) .) . verifyRealTimeUpdateNotifications


-- | When data changes and there's a valid subscription, Facebook
-- will @POST@ to your 'RealTimeUpdateUrl' with a JSON-encoded
-- object containing the notifications.  A
-- 'RealTimeUpdateNotification a' represents such object where
-- 'a' is type of the entries (e.g.,
-- 'RealTimeUpdateNotificationUserEntry').
--
-- If you have a single 'RealTimeUpdateUrl' for different kinds
-- of notifications, you may parse a @RealTimeUpdateNotification
-- 'A.Value'@ and then manually parse the 'A.Value' depending on
-- the value of 'rtunObject'.
--
-- We recommend using 'getRealTimeUpdateNotifications'.
data RealTimeUpdateNotification a =
  RealTimeUpdateNotification {
    rtunObject  :: RealTimeUpdateObject
  , rtunEntries :: [a]
  } deriving (Eq, Ord, Show, Typeable)

instance A.FromJSON a => A.FromJSON (RealTimeUpdateNotification a) where
  parseJSON (A.Object v) =
    RealTimeUpdateNotification
      <$> v A..: "object"
      <*> v A..: "entry"
  parseJSON _ = mzero


-- | A notification for the 'UserRTUO' object.
data RealTimeUpdateNotificationUserEntry =
  RealTimeUpdateNotificationUserEntry {
    rtuneUserId        :: Id
  , rtuneChangedFields :: [RealTimeUpdateField]
  , rtuneTime          :: Integer
  } deriving (Eq, Ord, Show, Typeable)

instance A.FromJSON RealTimeUpdateNotificationUserEntry where
  parseJSON (A.Object v) =
    RealTimeUpdateNotificationUserEntry
      <$> v A..: "uid"
      <*> fmap (map encodeUtf8) (v A..: "changed_fields")
      <*> v A..: "time"

  parseJSON _ = mzero
