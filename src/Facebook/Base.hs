{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings, CPP #-}
module Facebook.Base
    ( fbreq
    , ToSimpleQuery(..)
    , asJson
    , asJsonHelper
    , asBS
    , FacebookException(..)
    , fbhttp
    , fbhttpHelper
    , httpCheck
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8 (ByteString)
import Data.Default (def)
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified Control.Exception.Lifted as E
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Char8 as AT
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

#if DEBUG
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as L
#endif


import Facebook.Types
import Facebook.Monad


-- | A plain 'H.Request' to a Facebook API.  Use this instead of
-- 'def' when creating new 'H.Request'@s@ for Facebook.
fbreq :: Monad m =>
         Text                        -- ^ Path.
      -> Maybe (AccessToken anyKind) -- ^ Access token.
      -> HT.SimpleQuery              -- ^ Parameters.
      -> FacebookT anyAuth m H.Request
fbreq path mtoken query =
    withTier $ \tier ->
      let host = case tier of
                   Production -> "graph.facebook.com"
                   Beta ->  "graph.beta.facebook.com"
      in def { H.secure        = True
             , H.host          = host
             , H.port          = 443
             , H.path          = TE.encodeUtf8 path
             , H.redirectCount = 3
             , H.queryString   =
                 HT.renderSimpleQuery False $
                 maybe id tsq mtoken query
             , H.responseTimeout = Just 120000000 -- 2 minutes
             }


-- | Internal class for types that may be passed on queries to
-- Facebook's API.
class ToSimpleQuery a where
    -- | Prepend to the given query the parameters necessary to
    -- pass this data type to Facebook.
    tsq :: a -> HT.SimpleQuery -> HT.SimpleQuery
    tsq _ = id

instance ToSimpleQuery Credentials where
    tsq creds = (:) ("client_id",     appIdBS     creds) .
                (:) ("client_secret", appSecretBS creds)

instance ToSimpleQuery (AccessToken anyKind) where
    tsq token = (:) ("access_token", TE.encodeUtf8 $ accessTokenData token)


-- | Converts a plain 'H.Response' coming from 'H.http' into a
-- JSON value.
asJson :: (MonadIO m, MonadTrans t, R.MonadThrow m, A.FromJSON a) =>
          H.Response (C.ResumableSource m ByteString)
       -> t m a
asJson = lift . asJsonHelper

asJsonHelper :: (MonadIO m, R.MonadThrow m, A.FromJSON a) =>
                H.Response (C.ResumableSource m ByteString)
             -> m a
asJsonHelper response = do
#if DEBUG
  bs <- H.responseBody response C.$$+- fmap L.fromChunks CL.consume
  _ <- liftIO $ printf "asJsonHelper: %s\n" (show bs)
  val <- either (fail . ("asJsonHelper: A.decode returned " ++)) return (A.eitherDecode bs)
#else
  val <- H.responseBody response C.$$+- C.sinkParser A.json'
#endif
  case A.fromJSON val of
    A.Success r -> return r
    A.Error str ->
        E.throw $ FbLibraryException $ T.concat
             [ "Facebook.Base.asJson: could not parse "
             , " Facebook's response as a JSON value ("
             , T.pack str, ")" ]


-- | Converts a plain 'H.Response' into a string 'ByteString'.
asBS :: (Monad m) =>
        H.Response (C.ResumableSource m ByteString)
     -> FacebookT anyAuth m ByteString
asBS response = lift $ H.responseBody response C.$$+- fmap B.concat CL.consume


-- | An exception that may be thrown by functions on this
-- package.  Includes any information provided by Facebook.
data FacebookException =
    -- | An exception coming from Facebook.
    FacebookException { fbeType    :: Text
                      , fbeMessage :: Text
                      }
    -- | An exception coming from the @fb@ package's code.
  | FbLibraryException { fbeMessage :: Text }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON FacebookException where
    parseJSON (A.Object v) =
        FacebookException <$> v A..: "type"
                          <*> v A..: "message"
    parseJSON _ = mzero

instance E.Exception FacebookException where


-- | Same as 'H.http', but tries to parse errors and throw
-- meaningful 'FacebookException'@s@.
fbhttp :: (MonadBaseControl IO m, R.MonadResource m) =>
          H.Request
       -> FacebookT anyAuth m (H.Response (C.ResumableSource m ByteString))
fbhttp req = do
  manager <- getManager
  lift (fbhttpHelper manager req)

fbhttpHelper :: (MonadBaseControl IO m, R.MonadResource m) =>
                H.Manager
             -> H.Request
             -> m (H.Response (C.ResumableSource m ByteString))
fbhttpHelper manager req = do
  let req' = req { H.checkStatus = \_ _ _ -> Nothing }
#if DEBUG
  _ <- liftIO $ printf "fbhttp doing request\n\tmethod: %s\n\tsecure: %s\n\thost: %s\n\tport: %s\n\tpath: %s\n\tqueryString: %s\n\trequestHeaders: %s\n" (show $ H.method req') (show $ H.secure req') (show $ H.host req') (show $ H.port req') (show $ H.path req') (show $ H.queryString req') (show $ H.requestHeaders req')
#endif
  response <- H.http req' manager
  let status  = H.responseStatus    response
      headers = H.responseHeaders   response
      cookies = H.responseCookieJar response
#if DEBUG
  _ <- liftIO $ printf "fbhttp response status: %s\n" (show status)
#endif
  if isOkay status
    then return response
    else do
      let statusexc = H.StatusCodeException status headers cookies
      val <- E.try $ asJsonHelper response
      case val :: Either E.SomeException FacebookException of
        Right fbexc -> E.throw fbexc
        Left _ -> do
          case AT.parse wwwAuthenticateParser <$>
               lookup "WWW-Authenticate" headers of
            Just (AT.Done _ fbexc) -> E.throwIO fbexc
            _                      -> E.throwIO statusexc


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
httpCheck :: (MonadBaseControl IO m, R.MonadResource m) =>
             H.Request
          -> FacebookT anyAuth m Bool

httpCheck req = runResourceInFb $ do
  manager <- getManager
  let req' = req { H.method      = HT.methodHead
                 , H.checkStatus = \_ _ _ -> Nothing }
  isOkay . H.responseStatus <$> lift (H.httpLbs req' manager)
  -- Yes, we use httpLbs above so that we don't have to worry
  -- about consuming the responseBody.  Note that the
  -- responseBody should be empty since we're using HEAD, but
  -- I don't know if this is guaranteed.


-- | @True@ if the the 'Status' is ok (i.e. @2XX@).
isOkay :: HT.Status -> Bool
isOkay status =
  let sc = HT.statusCode status
  in 200 <= sc && sc < 300
