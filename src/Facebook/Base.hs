module Facebook.Base
    ( Credentials(..)
    , AccessToken(..)
    , User
    , App
    , fbreq
    , ToSimpleQuery(..)
    , asJson
    , asJson'
    , FacebookException(..)
    , fbhttp
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Network.HTTP.Types (Ascii)

import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


-- | Credentials that you get for your app when you register on
-- Facebook.
data Credentials =
    Credentials { clientId     :: Ascii -- ^ Your application ID.
                , clientSecret :: Ascii -- ^ Your application secret key.
                }
    deriving (Eq, Ord, Show, Typeable)


-- | An access token.  While you can make some API calls without
-- an access token, many require an access token and some will
-- give you more information with an appropriate access token.
--
-- There are two kinds of access tokens:
--
-- [User access token] An access token obtained after an user
-- accepts your application.  Let's you access more information
-- about that user and act on their behalf (depending on which
-- permissions you've asked for).
--
-- [App access token] An access token that allows you to take
-- administrative actions for your application.
--
-- These access tokens are distinguished by the phantom type on
-- 'AccessToken', which can be 'User' or 'App'.
data AccessToken kind =
    AccessToken { accessTokenData    :: Ascii
                  -- ^ The access token itself.
                , accessTokenExpires :: Maybe UTCTime
                  -- ^ Expire time of the access token.  It may
                  -- never expire, in case it will be @Nothing@.
                }
    deriving (Eq, Ord, Show, Typeable)

-- | Phantom type used mark an 'AccessToken' as an user access
-- token.
data User

-- | Phantom type used mark an 'AccessToken' as an app access
-- token.
data App


-- | A plain 'H.Request' to a Facebook API.  Use this instead of
-- 'H.def' when creating new 'H.Request'@s@ for Facebook.
fbreq :: HT.Ascii -> Maybe (AccessToken kind) -> HT.SimpleQuery -> H.Request m
fbreq path mtoken query =
    H.def { H.secure        = True
          , H.host          = "graph.facebook.com"
          , H.port          = 443
          , H.path          = path
          , H.redirectCount = 3
          , H.queryString   =
              HT.renderSimpleQuery False $
              maybe id tsq mtoken query
          }


-- | Class for types that may be passed on queries to Facebook's
-- API.
class ToSimpleQuery a where
    -- | Prepend to the given query the parameters necessary to
    -- pass this data type to Facebook.
    tsq :: a -> HT.SimpleQuery -> HT.SimpleQuery
    tsq _ = id

instance ToSimpleQuery Credentials where
    tsq creds = (:) ("client_id",     clientId     creds) .
                (:) ("client_secret", clientSecret creds)

instance ToSimpleQuery (AccessToken kind) where
    tsq token = (:) ("access_token", accessTokenData token)


-- | Converts a plain 'H.Response' coming from 'H.http' into a
-- response with a JSON value.
asJson :: (C.ResourceThrow m, C.BufferSource bsrc, A.FromJSON a) =>
          H.Response (bsrc m ByteString)
       -> C.ResourceT m (H.Response a)
asJson (H.Response status headers body) = do
  val <- body C.$$ C.sinkParser A.json'
  case A.fromJSON val of
    A.Error str -> fail $ "Facebook.Base.asJson: " ++ str
    A.Success r -> return (H.Response status headers r)


-- | Same as 'asJson', but returns only the JSON value.
asJson' :: (C.ResourceThrow m, C.BufferSource bsrc, A.FromJSON a) =>
           H.Response (bsrc m ByteString)
        -> C.ResourceT m a
asJson' = fmap H.responseBody . asJson


-- | An exception that may be thrown by functions on this module.
-- Includes any information provided by Facebook.
data FacebookException =
    FacebookException { fbeType    :: Text
                      , fbeMessage :: Text
                      }
    deriving (Eq, Ord, Show, Typeable)

instance A.FromJSON FacebookException where
    parseJSON (A.Object v) =
        FacebookException <$> v A..: "type"
                          <*> v A..: "message"
    parseJSON _ = mzero

instance E.Exception FacebookException where


-- | Same as 'H.http', but tries to parse errors and throw
-- meaningful 'FacebookException'@s@.
fbhttp :: C.ResourceIO m =>
          H.Request m
       -> H.Manager
       -> C.ResourceT m (H.Response (C.BufferedSource m ByteString))
fbhttp req manager = do
  let req' = req { H.checkStatus = \_ _ -> Nothing }
  response@(H.Response status headers _) <- H.http req' manager
  let sc = HT.statusCode status
  if 200 <= sc && sc < 300
    then return response
    else do
      let statusexc = H.StatusCodeException status headers
      val <- E.try $ asJson' response
      case val :: Either E.SomeException FacebookException of
        Left  _     -> E.throw statusexc
        Right fbexc -> E.throw fbexc
