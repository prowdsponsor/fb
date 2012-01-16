module Facebook.Base
    ( Credentials(..)
    , AccessToken(..)
    , User
    , App
    , fbreq
    , ToSimpleQuery(..)
    , asJson
    , asJson'
    ) where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Data.Typeable (Typeable)
import Data.Time (UTCTime)
import Network.HTTP.Types (Ascii)

import qualified Data.Aeson as A
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as H


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
fbreq :: H.Ascii -> Maybe (AccessToken kind) -> H.SimpleQuery -> H.Request m
fbreq path mtoken query =
    H.def { H.secure        = True
          , H.host          = "graph.facebook.com"
          , H.port          = 443
          , H.path          = path
          , H.redirectCount = 3
          , H.queryString   =
              H.renderSimpleQuery False $
              maybe id tsq mtoken query
          }


-- | Class for types that may be passed on queries to Facebook's
-- API.
class ToSimpleQuery a where
    -- | Prepend to the given query the parameters necessary to
    -- pass this data type to Facebook.
    tsq :: a -> H.SimpleQuery -> H.SimpleQuery
    tsq _ = id

instance ToSimpleQuery Credentials where
    tsq creds = (:) ("client_id",     clientId     creds) .
                (:) ("client_secret", clientSecret creds)

instance ToSimpleQuery (AccessToken kind) where
    tsq token = (:) ("access_token", accessTokenData token)


-- | Converts a plain 'H.Response' coming from 'H.http' into a
-- response with a JSON value.
asJson :: (C.ResourceThrow m, C.BufferSource bsrc) =>
          H.Response (bsrc m ByteString)
       -> C.ResourceT m (H.Response A.Value)
asJson (H.Response status headers body) =
  H.Response status headers <$> (body C.$$ C.sinkParser A.json')


-- | Same as 'asJson', but returns only the JSON value.
asJson' :: (C.ResourceThrow m, C.BufferSource bsrc) =>
           H.Response (bsrc m ByteString)
        -> C.ResourceT m A.Value
asJson' = fmap H.responseBody . asJson
