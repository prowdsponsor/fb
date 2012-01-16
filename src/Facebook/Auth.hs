module Facebook.Auth
    ( getAppAccessToken
    , getUserAccessTokenStep1
    , getUserAccessTokenStep2
    , RedirectUrl
    ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime, addUTCTime)

import qualified Data.Attoparsec.Char8 as A
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as H

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
      let req = fbreq "/oauth/access_token" Nothing $
                tsq creds [code, ("redirect_uri", TE.encodeUtf8 redirectUrl)]
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
