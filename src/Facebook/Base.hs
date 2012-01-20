module Facebook.Base
    ( Credentials(..)
    , AccessToken(..)
    , AccessTokenData
    , accessTokenData
    , accessTokenExpires
    , User
    , App
    , fbreq
    , ToSimpleQuery(..)
    , asJson
    , asJson'
    , FacebookException(..)
    , fbhttp
    , httpCheck
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable, Typeable1)
import Network.HTTP.Types (Ascii)

import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Char8 as AT
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


-- | Credentials that you get for your app when you register on
-- Facebook.
data Credentials =
    Credentials { appName   :: Ascii -- ^ Your application name (e.g. for OpenGraph calls).
                , appId     :: Ascii -- ^ Your application ID.
                , appSecret :: Ascii -- ^ Your application secret key.
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
data AccessToken kind where
    UserAccessToken :: AccessTokenData -> UTCTime -> AccessToken User
    AppAccessToken  :: AccessTokenData -> AccessToken App

deriving instance Eq   (AccessToken kind)
deriving instance Ord  (AccessToken kind)
deriving instance Show (AccessToken kind)
deriving instance Typeable1 AccessToken

-- | The access token data that is passed to Facebook's API
-- calls.
type AccessTokenData = Ascii

-- | Get the access token data.
accessTokenData :: AccessToken kind -> AccessTokenData
accessTokenData (UserAccessToken d _) = d
accessTokenData (AppAccessToken d)    = d

-- | Expire time of an access token.  It may never expire, in
-- which case it will be @Nothing@.
accessTokenExpires :: AccessToken kind -> Maybe UTCTime
accessTokenExpires (UserAccessToken _ expt) = Just expt
accessTokenExpires (AppAccessToken _)       = Nothing

-- | Phantom type used mark an 'AccessToken' as an user access
-- token.
data User deriving (Typeable)

-- | Phantom type used mark an 'AccessToken' as an app access
-- token.
data App deriving (Typeable)


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
    tsq creds = (:) ("client_id",     appId     creds) .
                (:) ("client_secret", appSecret creds)

instance ToSimpleQuery (AccessToken kind) where
    tsq token = (:) ("access_token", accessTokenData token)


-- | Converts a plain 'H.Response' coming from 'H.http' into a
-- response with a JSON value.
asJson :: (C.ResourceThrow m, C.IsSource bsrc, A.FromJSON a) =>
          H.Response (bsrc m ByteString)
       -> C.ResourceT m (H.Response a)
asJson (H.Response status headers body) = do
  val <- body C.$$ C.sinkParser A.json'
  case A.fromJSON val of
    A.Success r -> return (H.Response status headers r)
    A.Error str ->
        E.throw $ FbLibraryException $ T.concat
             [ "Facebook.Base.asJson: could not parse "
             , " Facebook's response as a JSON value ("
             , T.pack str, ")" ]


-- | Same as 'asJson', but returns only the JSON value.
asJson' :: (C.ResourceThrow m, C.IsSource bsrc, A.FromJSON a) =>
           H.Response (bsrc m ByteString)
        -> C.ResourceT m a
asJson' = fmap H.responseBody . asJson


-- | An exception that may be thrown by functions on this
-- package.  Includes any information provided by Facebook.
data FacebookException =
    -- | An exception coming from Facebook.
    FacebookException { fbeType    :: Text
                      , fbeMessage :: Text
                      }
    -- | An exception coming from the @fb@ package's code.
  | FbLibraryException { fbeMessage :: Text }
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
       -> C.ResourceT m (H.Response (C.Source m ByteString))
fbhttp req manager = do
  let req' = req { H.checkStatus = \_ _ -> Nothing }
  response@(H.Response status headers _) <- H.http req' manager
  if isOkay status
    then return response
    else do
      let statusexc = H.StatusCodeException status headers
      val <- E.try $ asJson' response
      case val :: Either E.SomeException FacebookException of
        Right fbexc -> E.throw fbexc
        Left _ -> do
          case AT.parse wwwAuthenticateParser <$>
               lookup "WWW-Authenticate" headers of
            Just (AT.Done _ fbexc) -> E.throw fbexc
            _                      -> E.throw statusexc


-- | Try to parse the @WWW-Authenticate@ header of a Facebook
-- response.
wwwAuthenticateParser :: AT.Parser FacebookException
wwwAuthenticateParser =
    FacebookException <$  AT.string "OAuth \"Facebook Platform\" "
                      <*> text
                      <*  AT.char ' '
                      <*> text
    where
      text  = T.pack <$ AT.char '"' <*> many tchar <* AT.char '"'
      tchar = (AT.char '\\' *> AT.anyChar) <|> AT.notChar '"'


-- | Send a @HEAD@ request just to see if the resposne status
-- code is 2XX (returns @True@) or not (returns @False@).
httpCheck :: C.ResourceIO m =>
             H.Request m
          -> H.Manager
          -> C.ResourceT m Bool
httpCheck req manager = do
  let req' = req { H.method      = HT.methodHead
                 , H.checkStatus = \_ _ -> Nothing }
  H.Response status _ _ <- H.httpLbs req' manager
  return $! isOkay status
  -- Yes, we use httpLbs above so that we don't have to worry
  -- about consuming the responseBody.  Note that the
  -- responseBody should be empty since we're using HEAD, but
  -- I don't know if this is guaranteed.


-- | @True@ if the the 'Status' is ok (i.e. @2XX@).
isOkay :: HT.Status -> Bool
isOkay status =
  let sc = HT.statusCode status
  in 200 <= sc && sc < 300
