module Facebook.Base
    ( fbreq
    , ToSimpleQuery(..)
    , asJson
    , asBS
    , FacebookException(..)
    , fbhttp
    , httpCheck
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Char8 as AT
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import Facebook.Types
import Facebook.Monad


-- | A plain 'H.Request' to a Facebook API.  Use this instead of
-- 'H.def' when creating new 'H.Request'@s@ for Facebook.
fbreq :: HT.Ascii -> Maybe (AccessToken anyKind) -> HT.SimpleQuery -> H.Request m
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


-- | Internal class for types that may be passed on queries to
-- Facebook's API.
class ToSimpleQuery a where
    -- | Prepend to the given query the parameters necessary to
    -- pass this data type to Facebook.
    tsq :: a -> HT.SimpleQuery -> HT.SimpleQuery
    tsq _ = id

instance ToSimpleQuery Credentials where
    tsq creds = (:) ("client_id",     appId     creds) .
                (:) ("client_secret", appSecret creds)

instance ToSimpleQuery (AccessToken anyKind) where
    tsq token = (:) ("access_token", accessTokenData token)


-- | Converts a plain 'H.Response' coming from 'H.http' into a
-- JSON value.
asJson :: (C.ResourceThrow m, C.IsSource bsrc, A.FromJSON a) =>
          H.Response (bsrc m ByteString)
       -> FacebookT anyAuth (C.ResourceT m) a
asJson response = do
  val <- lift $ H.responseBody response C.$$ C.sinkParser A.json'
  case A.fromJSON val of
    A.Success r -> return r
    A.Error str ->
        E.throw $ FbLibraryException $ T.concat
             [ "Facebook.Base.asJson: could not parse "
             , " Facebook's response as a JSON value ("
             , T.pack str, ")" ]

-- | Converts a plain 'H.Response' into a string 'ByteString'.
asBS :: (C.ResourceThrow m, C.IsSource bsrc) =>
        H.Response (bsrc m ByteString)
     -> FacebookT anyAuth (C.ResourceT m) ByteString
asBS response = lift $ H.responseBody response C.$$ fmap B.concat CL.consume



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
       -> FacebookT anyAuth (C.ResourceT m) (H.Response (C.Source m ByteString))
fbhttp req = do
  manager <- getManager
  let req' = req { H.checkStatus = \_ _ -> Nothing }
  response@(H.Response status headers _) <- lift (H.http req' manager)
  if isOkay status
    then return response
    else do
      let statusexc = H.StatusCodeException status headers
      val <- E.try $ asJson response
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
          -> FacebookT anyAuth m Bool
httpCheck req = runResourceInFb $ do
  manager <- getManager
  let req' = req { H.method      = HT.methodHead
                 , H.checkStatus = \_ _ -> Nothing }
  H.Response status _ _ <- lift (H.httpLbs req' manager)
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
