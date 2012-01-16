module Facebook.Base
    ( Credentials(..)
    , AccessToken(..)
    , User
    , App
    , fbreq
    , rsq
    , ToSimpleQuery(..)
    , getAppAccessToken
    , getUserAccessTokenStep1
    , getUserAccessTokenStep2
    , RedirectUrl
    ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString)
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Network.HTTP.Types (Ascii)

import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
fbreq :: H.Request m
fbreq =
    H.def { H.secure        = True
          , H.host          = "graph.facebook.com"
          , H.port          = 443
          , H.redirectCount = 3
          }


-- | Same as @'H.renderSimpleQuery' False@, but easier to type.
rsq :: H.SimpleQuery -> H.Ascii
rsq = H.renderSimpleQuery False


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


-- | Get an app access token from Facebook using your
-- credentials.
getAppAccessToken :: C.ResourceIO m =>
                     Credentials
                  -> H.Manager
                  -> C.ResourceT m (AccessToken App)
getAppAccessToken creds manager = do
  let req = fbreq { H.path        = "/oauth/access_token"
                  , H.queryString = rsq $ tsq creds [("grant_type", "client_credentials")]
                  }
  response <- H.http req manager
  H.responseBody response C.$$
    C.sinkParser (AccessToken <$  A.string "access_token="
                              <*> A.takeByteString
                              <*> pure Nothing)


-- | The first step to get an user access token.  Returns the
-- Facebook URL you should redirect you user to.  Facebook will
-- authenticate the user, authorize your app and then redirect
-- the user back into the provider 'RedirectUrl'.
getUserAccessTokenStep1 :: Credentials -> RedirectUrl -> Text
getUserAccessTokenStep1 creds redirectUrl =
  T.concat [ "https://www.facebook.com/dialog/oauth?client_id="
           , TE.decodeUtf8 (clientId creds)
           , "&redirect_uri="
           , redirectUrl
           ]


-- | The second step to get an user access token.  If the user is
-- successfully authenticate and they authorize your application,
-- then they'll be redirected back to the 'RedirectUrl' you've
-- passed to 'getUserAccessTokenStep1'.  You should take the
-- request query parameters passed to your 'RedirectUrl' and give
-- to this function that will complete the user authentication
-- flow and give you an @'AccessToken' 'User'@.
getUserAccessTokenStep2 :: C.ResourceIO m =>
                           Credentials
                        -> RedirectUrl -- ^ Exactly the same as in 'getUserAccessTokenStep1'.
                        -> H.SimpleQuery
                        -> H.Manager
                        -> C.ResourceT m (AccessToken User)
getUserAccessTokenStep2 creds redirectUrl query manager =
  case query of
    [code@("code", _)] -> do
      now <- liftIO getCurrentTime
      let req = fbreq { H.path        = "/oauth/access_token"
                      , H.queryString =
                          rsq $ tsq creds
                          [code, ("redirect_uri", TE.encodeUtf8 redirectUrl)]
                      }
      let toExpire i = Just (addUTCTime (fromIntegral (i :: Int)) now)
      response <- H.http req manager
      H.responseBody response C.$$
        C.sinkParser (AccessToken <$  A.string "access_token="
                                  <*> A.takeWhile (/= '?')
                                  <*  A.string "&expires="
                                  <*> (toExpire <$> A.decimal)
                                  <*  A.endOfInput)
    _ -> -- FIXME: Better error handling
         fail $ "getUserAccessTokenStep2: " ++ show query


-- | URL where the user is redirected to after Facebook
-- authenticates the user authorizes your application.  This URL
-- should be inside the domain registered for your Facebook
-- application.
type RedirectUrl = Text