{-# LANGUAGE FlexibleContexts, GADTs, ScopedTypeVariables, OverloadedStrings #-}
module Facebook.Auth
    ( getAppAccessToken
    , getUserAccessTokenStep1
    , getUserAccessTokenStep2
    , getUserLogoutUrl
    , extendUserAccessToken
    , RedirectUrl
    , Permission
    , unPermission
    , hasExpired
    , isValid
    , parseSignedRequest
    , debugToken
    , DebugToken(..)
    ) where

import Control.Applicative
import Control.Monad (guard, join, liftM, mzero)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.Classes (constTimeEq)
import Crypto.Hash.CryptoAPI (SHA256)
import Crypto.HMAC (hmac', MacKey(..))
import Data.Aeson ((.:))
import Data.Aeson.Parser (json')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime, addUTCTime, UTCTime)
import Data.Typeable (Typeable)
import Data.String (IsString(..))

import qualified Control.Exception.Lifted as E
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.Attoparsec.Char8 as AB
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Text as CT
import qualified Data.List as L
import qualified Data.Serialize as Cereal
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import Facebook.Types
import Facebook.Base
import Facebook.Monad


-- | Get an app access token from Facebook using your
-- credentials.
getAppAccessToken :: (R.MonadResource m, MonadBaseControl IO m) =>
                     FacebookT Auth m AppAccessToken
getAppAccessToken =
  runResourceInFb $ do
    creds <- getCreds
    req   <- fbreq "/oauth/access_token" Nothing $
             tsq creds [("grant_type", "client_credentials")]
    response <- fbhttp req
    lift $
      H.responseBody response C.$$+-
      CT.decode CT.utf8 C.=$
      C.sinkParser (AppAccessToken <$  A.string "access_token="
                                   <*> A.takeText)


-- | The first step to get an user access token.  Returns the
-- Facebook URL you should redirect you user to.  Facebook will
-- authenticate the user, authorize your app and then redirect
-- the user back into the provider 'RedirectUrl'.
getUserAccessTokenStep1 :: Monad m =>
                           RedirectUrl
                        -> [Permission]
                        -> FacebookT Auth m Text
getUserAccessTokenStep1 redirectUrl perms = do
  creds <- getCreds
  withTier $ \tier ->
    let urlBase = case tier of
                    Production -> "https://www.facebook.com/dialog/oauth?client_id="
                    Beta ->  "https://www.beta.facebook.com/dialog/oauth?client_id="
    in T.concat $ urlBase
                : appId creds
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
-- flow and give you an @'UserAccessToken'@.
getUserAccessTokenStep2 :: (MonadBaseControl IO m, R.MonadResource m) =>
                           RedirectUrl -- ^ Should be exactly the same
                                       -- as in 'getUserAccessTokenStep1'.
                        -> [Argument]  -- ^ Query parameters.
                        -> FacebookT Auth m UserAccessToken
getUserAccessTokenStep2 redirectUrl query =
  case query of
    [code@("code", _)] -> runResourceInFb $ do
      -- Get the access token data through Facebook's OAuth.
      now   <- liftIO getCurrentTime
      creds <- getCreds
      req   <- fbreq "/oauth/access_token" Nothing $
               tsq creds [code, ("redirect_uri", TE.encodeUtf8 redirectUrl)]
      preToken <- fmap (userAccessTokenParser now) . asBS =<< fbhttp req

      -- Get user's ID throught Facebook's graph.
      userInfo <- asJson =<< fbhttp =<< fbreq "/me" (Just preToken) [("fields", "id")]
      case (AE.parseEither (.: "id") userInfo, preToken) of
        (Left str, _) ->
            E.throw $ FbLibraryException $ T.concat
                 [ "getUserAccessTokenStep2: failed to get the UserId ("
                 , T.pack str, ")" ]
        (Right (userId :: UserId), UserAccessToken _ d e) ->
            return (UserAccessToken userId d e)

    _ -> let [error_, errorReason, errorDescr] =
                 map (fromMaybe "" . flip lookup query)
                     ["error", "error_reason", "error_description"]
             errorType = T.concat [t error_, " (", t errorReason, ")"]
             t = TE.decodeUtf8With TE.lenientDecode
         in E.throw $ FacebookAuthException errorType (t errorDescr)


-- | Attoparsec parser for user access tokens returned by
-- Facebook as a query string.  Returns an user access token with
-- a broken 'UserId'.
userAccessTokenParser :: UTCTime -- ^ 'getCurrentTime'
                      -> B.ByteString
                      -> UserAccessToken
userAccessTokenParser now bs =
    let q = HT.parseQuery bs; lookup' a = join (lookup a q)
    in case (,) <$> lookup' "access_token" <*> lookup' "expires" of
         (Just (tok, expt)) -> UserAccessToken userId (dec tok) (toExpire expt)
         _ -> error $ "userAccessTokenParser: failed to parse " ++ show bs
       where toExpire expt = let i = read (B8.unpack expt) :: Int
                             in addUTCTime (fromIntegral i) now
             userId = error "userAccessTokenParser: never here"
             dec = TE.decodeUtf8With TE.lenientDecode


-- | The URL an user should be redirected to in order to log them
-- out of their Facebook session.  Facebook will then redirect
-- the user to the provided URL after logging them out.  Note
-- that, at the time of this writing, Facebook's policies require
-- you to log the user out of Facebook when they ask to log out
-- of your site.
--
-- Note also that Facebook may refuse to redirect the user to the
-- provided URL if their user access token is invalid.  In order
-- to prevent this bug, we suggest that you use 'isValid' before
-- redirecting the user to the URL provided by 'getUserLogoutUrl'
-- since this function doesn't do any validity checks.
getUserLogoutUrl :: Monad m =>
                    UserAccessToken
                    -- ^ The user's access token.
                 -> RedirectUrl
                    -- ^ URL the user should be directed to in
                    -- your site domain.
                 -> FacebookT Auth m Text
                    -- ^ Logout URL in
                    -- @https:\/\/www.facebook.com\/@ (or on
                    -- @https:\/\/www.beta.facebook.com\/@ when
                    -- using the beta tier).
getUserLogoutUrl (UserAccessToken _ data_ _) next = do
  withTier $ \tier ->
    let urlBase = case tier of
                    Production -> "https://www.facebook.com/logout.php?"
                    Beta ->  "https://www.beta.facebook.com/logout.php?"
    in TE.decodeUtf8 $
         urlBase <>
         HT.renderQuery False [ ("next", Just (TE.encodeUtf8 next))
                              , ("access_token", Just (TE.encodeUtf8 data_)) ]


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
newtype Permission =
  Permission {
    unPermission :: Text
    -- ^ Retrieves the 'Text' back from a 'Permission'.  Most of
    -- the time you won't need to use this function, but you may
    -- need it if you're a library author.
  } deriving (Eq, Ord)

instance Show Permission where
    show = show . unPermission

instance IsString Permission where
    fromString = Permission . fromString


-- | @True@ if the access token has expired, otherwise @False@.
hasExpired :: (Functor m, MonadIO m) => AccessToken anyKind -> m Bool
hasExpired token =
  case accessTokenExpires token of
    Nothing      -> return False
    Just expTime -> (>= expTime) <$> liftIO getCurrentTime


-- | @True@ if the access token is valid.  An expired access
-- token is not valid (see 'hasExpired').  However, a non-expired
-- access token may not be valid as well.  For example, in the
-- case of an user access token, they may have changed their
-- password, logged out from Facebook or blocked your app.
isValid :: (MonadBaseControl IO m, R.MonadResource m) =>
           AccessToken anyKind
        -> FacebookT anyAuth m Bool
isValid token = do
  expired <- hasExpired token
  if expired
    then return False
    else
      let page = case token of
                   UserAccessToken _ _ _ -> "/me"
                   -- Documented way of checking if the token is valid,
                   -- see <https://developers.facebook.com/blog/post/500/>.
                   AppAccessToken _ -> "/19292868552"
                   -- This is Facebook's page on Facebook.  While
                   -- this behaviour is undocumented, it will
                   -- return a "400 Bad Request" status code
                   -- whenever the access token is invalid.  It
                   -- will actually work with user access tokens,
                   -- too, but they have another, better way of
                   -- being checked.
      in httpCheck =<< fbreq page (Just token) []


-- | Extend the expiration time of an user access token (see
-- <https://developers.facebook.com/docs/offline-access-deprecation/>,
-- <https://developers.facebook.com/roadmap/offline-access-removal/>).
-- Only short-lived user access tokens may extended into
-- long-lived user access tokens, you must get a new short-lived
-- user access token if you need to extend a long-lived
-- one.  Returns @Left exc@ if there is an error while extending,
-- or @Right token@ with the new user access token (which could
-- have the same data and expiration time as before, but you
-- can't assume this).  Note that expired access tokens can't be
-- extended, only valid tokens.
extendUserAccessToken :: (MonadBaseControl IO m, R.MonadResource m) =>
                         UserAccessToken
                      -> FacebookT Auth m (Either FacebookException UserAccessToken)
extendUserAccessToken token@(UserAccessToken uid data_ _)
    = do expired <- hasExpired token
         if expired then return (Left hasExpiredExc) else tryToExtend
    where
      tryToExtend = runResourceInFb $ do
        creds <- getCreds
        req   <- fbreq "/oauth/access_token" Nothing $
                 tsq creds [ ("grant_type", "fb_exchange_token")
                           , ("fb_exchange_token", TE.encodeUtf8 data_) ]
        eresponse <- E.try (asBS =<< fbhttp req)
        case eresponse of
          Right response -> do
            now <- liftIO getCurrentTime
            return (Right $ case userAccessTokenParser now response of
                              UserAccessToken _ data' expires' ->
                                UserAccessToken uid data' expires')
          Left exc -> return (Left exc)

      hasExpiredExc =
          mkExc [ "the user access token has already expired, "
                , "so I'll not try to extend it." ]
      mkExc = FbLibraryException . T.concat . ("extendUserAccessToken: ":)


-- | Parses a Facebook signed request
-- (<https://developers.facebook.com/docs/authentication/signed_request/>),
-- verifies its authencity and integrity using the HMAC and
-- decodes its JSON object.
parseSignedRequest :: (AE.FromJSON a, Monad m) =>
                      B8.ByteString -- ^ Encoded Facebook signed request
                   -> FacebookT Auth m (Maybe a)
parseSignedRequest signedRequest =
  runMaybeT $ do
    -- Split, decode and JSON-parse
    let (encodedSignature, encodedUnparsedPayloadWithDot) = B8.break (== '.') signedRequest
    ('.', encodedUnparsedPayload) <- MaybeT $ return (B8.uncons encodedUnparsedPayloadWithDot)
    signature       <- eitherToMaybeT $ Base64URL.decode $ addBase64Padding encodedSignature
    unparsedPayload <- eitherToMaybeT $ Base64URL.decode $ addBase64Padding encodedUnparsedPayload
    payload         <- eitherToMaybeT $ AB.parseOnly json' unparsedPayload

    -- Verify signature
    SignedRequestAlgorithm algo <- fromJson payload
    guard (algo == "HMAC-SHA256")
    hmacKey <- credsToHmacKey `liftM` lift getCreds
    let expectedSignature = Cereal.encode $ hmac' hmacKey encodedUnparsedPayload
    guard (signature `constTimeEq` expectedSignature)

    -- Parse user data type
    fromJson payload

 where eitherToMaybeT :: Monad m => Either a b -> MaybeT m b
       eitherToMaybeT = MaybeT . return . either (const Nothing) Just
       fromJson :: (AE.FromJSON a, Monad m) => AE.Value -> MaybeT m a
       fromJson = eitherToMaybeT . AE.parseEither AE.parseJSON
       credsToHmacKey :: Credentials -> MacKey ctx SHA256
       credsToHmacKey = MacKey . appSecretBS

newtype SignedRequestAlgorithm = SignedRequestAlgorithm Text
instance AE.FromJSON SignedRequestAlgorithm where
  parseJSON (AE.Object v) = SignedRequestAlgorithm <$> v .: "algorithm"
  parseJSON _             = mzero


-- | The @base64-bytestring@ package provides two different
-- decoding functions for @base64url@: 'Base64URL.decode' and
-- 'Base64URL.decodeLenient'.  The former is too strict for us
-- since Facebook does add padding to its signed requests, but
-- the latter is too lenient and will accept *anything*.
--
-- Instead of being too lenient, we just use this function add
-- the padding base to the encoded string, thus allowing
-- 'Base64URL.decode' to chew it.
addBase64Padding :: B.ByteString -> B.ByteString
addBase64Padding bs
  | drem == 2 = bs `B.append` "=="
  | drem == 3 = bs `B.append` "="
  | otherwise = bs
  where drem = B.length bs `mod` 4


-- | Get detailed information about an access token.
debugToken :: (MonadBaseControl IO m, R.MonadResource m) =>
              AppAccessToken  -- ^ Your app access token.
           -> AccessTokenData -- ^ The access token you want to debug.
           -> FacebookT Auth m DebugToken
debugToken appToken userTokenData  = do
  req <- fbreq "/debug_token" (Just appToken) $
           [ ("input_token", TE.encodeUtf8 userTokenData) ]
  ret <- undata <$> (asJson =<< fbhttp req)
  let muserToken = UserAccessToken <$> dtUserId ret
                                   <*> return userTokenData
                                   <*> dtExpiresAt ret
  return ret { dtAccessToken = muserToken }


-- | Helper used in 'debugToken'.  Unfortunately, we can't use 'Pager' here.
data Undata a =
  Undata {
    undata :: a
  }

instance AE.FromJSON a => AE.FromJSON (Undata a) where
  parseJSON (AE.Object v) =
    Undata <$> v AE..: "data"
  parseJSON _ = mzero


-- | Detailed information about an access token (cf. 'debugToken').
data DebugToken =
  DebugToken
    { dtAppId       :: Maybe Text
    , dtAppName     :: Maybe Text
    , dtExpiresAt   :: Maybe UTCTime
    , dtIsValid     :: Maybe Bool
    , dtIssuedAt    :: Maybe UTCTime
    , dtScopes      :: Maybe [Permission]
    , dtUserId      :: Maybe Id
    , dtAccessToken :: Maybe UserAccessToken
    } deriving (Eq, Ord, Show, Typeable)


-- | Note: this instance always sets 'dtAccessToken' to
-- 'Nothing', but 'debugToken' will update this field before
-- returning the final 'DebugToken'.  This is done because we
-- need the 'AccessTokenData', which is not part of FB's
-- response.
instance AE.FromJSON DebugToken where
  parseJSON (AE.Object v) =
    DebugToken <$> (fmap idCode <$> v AE..:? "app_id")
               <*> v AE..:? "application"
               <*> (fmap unFbUTCTime <$> v AE..:? "expires_at")
               <*> v AE..:? "is_valid"
               <*> (fmap unFbUTCTime <$> v AE..:? "issued_at")
               <*> (fmap (map Permission) <$> v AE..:? "scopes")
               <*> v AE..:? "user_id"
               <*> pure Nothing
  parseJSON _ = mzero
