module Facebook.Auth
    ( getAppAccessToken
    , getUserAccessTokenStep1
    , getUserAccessTokenStep2
    , RedirectUrl
    , Permission
    , hasExpired
    , isValid
    ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime, addUTCTime)
import Data.String (IsString(..))

import qualified Control.Exception.Lifted as E
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Facebook.Base


-- | Get an app access token from Facebook using your
-- credentials.
getAppAccessToken :: C.ResourceIO m =>
                     Credentials
                  -> H.Manager
                  -> C.ResourceT m (AccessToken App)
getAppAccessToken creds manager = do
  let req = fbreq "/oauth/access_token" Nothing $
            tsq creds [("grant_type", "client_credentials")]
  response <- fbhttp req manager
  H.responseBody response C.$$
    C.sinkParser (AccessToken <$  A.string "access_token="
                              <*> A.takeByteString
                              <*> pure Nothing)


-- | The first step to get an user access token.  Returns the
-- Facebook URL you should redirect you user to.  Facebook will
-- authenticate the user, authorize your app and then redirect
-- the user back into the provider 'RedirectUrl'.
getUserAccessTokenStep1 :: Credentials
                        -> RedirectUrl
                        -> [Permission]
                        -> Text
getUserAccessTokenStep1 creds redirectUrl perms =
  T.concat $ "https://www.facebook.com/dialog/oauth?client_id="
           : TE.decodeUtf8 (clientId creds)
           : "&redirect_uri="
           : redirectUrl
           : (case perms of
                [] -> []
                _  -> "&scope=" : L.intersperse "," (map unPermission perms)
             )


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
                        -> HT.SimpleQuery
                        -> H.Manager
                        -> C.ResourceT m (AccessToken User)
getUserAccessTokenStep2 creds redirectUrl query manager =
  case query of
    [code@("code", _)] -> do
      now <- liftIO getCurrentTime
      let req = fbreq "/oauth/access_token" Nothing $
                tsq creds [code, ("redirect_uri", TE.encodeUtf8 redirectUrl)]
      let toExpire i = Just (addUTCTime (fromIntegral (i :: Int)) now)
      response <- fbhttp req manager
      H.responseBody response C.$$
        C.sinkParser (AccessToken <$  A.string "access_token="
                                  <*> A.takeWhile (/= '?')
                                  <*  A.string "&expires="
                                  <*> (toExpire <$> A.decimal)
                                  <*  A.endOfInput)
    _ -> let [error_, errorReason, errorDescr] =
                 map (fromMaybe "" . flip lookup query)
                     ["error", "error_reason", "error_description"]
             errorType = T.concat [t error_, " (", t errorReason, ")"]
             t = TE.decodeUtf8With TE.lenientDecode
         in E.throw $ FacebookException errorType (t errorDescr)


-- | URL where the user is redirected to after Facebook
-- authenticates the user authorizes your application.  This URL
-- should be inside the domain registered for your Facebook
-- application.
type RedirectUrl = Text


-- | A permission that is asked for the user when he authorizes
-- your app.  Please refer to Facebook's documentation at
-- <https://developers.facebook.com/docs/reference/api/permissions/>
-- to see which permissions are available.
--
-- This is a @newtype@ of 'Text' that supports only 'IsString'.
-- This means that to create a 'Permission' you should use the
-- @OverloadedStrings@ language extension.  For example,
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > perms :: [Permission]
-- > perms = ["user_about_me", "email", "offline_access"]
newtype Permission = Permission { unPermission :: Text }

instance Show Permission where
    show = show . unPermission

instance IsString Permission where
    fromString = Permission . fromString


-- | @True@ if the access token has expired, otherwise @False@.
hasExpired :: (Functor m, MonadIO m) => AccessToken kind -> m Bool
hasExpired token =
  case accessTokenExpires token of
    Nothing      -> return False
    Just expTime -> (>= expTime) <$> liftIO getCurrentTime


-- | @True@ if the access token is valid.  An expired access
-- token is not valid (see 'hasExpired').  However, a non-expired
-- access token may not be valid as well.  For example, in the
-- case of an user access token, they may have changed their
-- password, logged out from Facebook or blocked your app.
isValid :: C.ResourceIO m =>
           AccessToken kind
        -> H.Manager
        -> C.ResourceT m Bool
isValid token manager = do
  expired <- hasExpired token
  if expired
    then return False
    else httpCheck (fbreq "/19292868552" (Just token) []) manager
    -- This is Facebook's own page.  While using an access token
    -- to access it shouldn't do much difference on most cases,
    -- when the access token is invalid a "400 Bad Request"
    -- status code is returned regardless.
