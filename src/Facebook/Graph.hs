{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Graph
    ( Id(..)
    , getObject
    , postObject
    , searchObjects
    , Pager(..)
    , fetchNextPage
    , fetchPreviousPage
    , fetchAllNextPages
    , fetchAllPreviousPages
    , (#=)
    , SimpleType(..)
    , Place(..)
    , Location(..)
    , GeoCoordinates(..)
    ) where


import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int8, Int16, Int32)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32, Word)
import System.Locale (defaultTimeLocale)

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE (fromValue)
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit as C
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Time as TI
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import Facebook.Types
import Facebook.Monad
import Facebook.Base


-- | The identification code of an object.
newtype Id = Id { idCode :: ByteString }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Id where
    parseJSON (A.Object v) = Id <$> v A..: "id"
    parseJSON other        = Id <$> A.parseJSON other


-- | Make a raw @GET@ request to Facebook's Graph API.
getObject :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
             ByteString     -- ^ Path (should begin with a slash @\/@)
          -> [Argument]     -- ^ Arguments to be passed to Facebook
          -> Maybe (AccessToken anyKind) -- ^ Optional access token
          -> FacebookT anyAuth m a
getObject path query mtoken =
  runResourceInFb $
    asJson =<< fbhttp =<< fbreq path mtoken query


-- | Make a raw @POST@ request to Facebook's Graph API.
postObject :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
              ByteString          -- ^ Path (should begin with a slash @\/@)
           -> [Argument]          -- ^ Arguments to be passed to Facebook
           -> AccessToken anyKind -- ^ Access token
           -> FacebookT Auth m a
postObject path query token =
  runResourceInFb $ do
    req <- fbreq path (Just token) query
    asJson =<< fbhttp req { H.method = HT.methodPost }


-- | Make a raw @GET@ request to the /search endpoint of Facebook’s
-- Graph API.  Returns a raw JSON 'A.Value'.
searchObjects :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a)
              => ByteString            -- ^ A Facebook object type to search for
              -> ByteString            -- ^ The keyword to search for
              -> [Argument]            -- ^ Additional arguments to pass
              -> Maybe UserAccessToken -- ^ Optional access token
              -> FacebookT anyAuth m (Pager a)
searchObjects objectType keyword query = getObject "/search" query'
  where query' = ("q", keyword) : ("type", objectType) : query


----------------------------------------------------------------------


-- | Many Graph API results are returned as a JSON object with
-- the following structure:
--
-- @
-- {
--   \"data\": [
--     ...item 1...,
--          :
--     ...item n...
--   ],
--   \"paging\": {
--     \"previous\": \"http://...link to previous page...\",
--     \"next\":     \"http://...link to next page...\"
--   }
-- }
-- @
--
-- Only the @\"data\"@ field is required, the others may or may
-- not appear.
--
-- A @Pager a@ datatype encodes such result where each item has
-- type @a@.  You may use functions 'fetchNextPage' and
-- 'fetchPreviousPage' to navigate through the results.
data Pager a =
  Pager {
      pagerData     :: [a]
    , pagerPrevious :: Maybe String
    , pagerNext     :: Maybe String
  } deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON a => A.FromJSON (Pager a) where
  parseJSON (A.Object v) =
    let paging f = v A..:? "paging" >>= maybe (return Nothing) (A..:? f)
    in Pager <$> v A..: "data"
             <*> paging "previous"
             <*> paging "next"
  parseJSON _ = mzero


-- | Tries to fetch the next page of a 'Pager'.  Returns
-- 'Nothing' whenever the current @Pager@ does not have a
-- 'pagerNext'.
fetchNextPage :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
                 Pager a -> FacebookT anyAuth m (Maybe (Pager a))
fetchNextPage = fetchHelper pagerNext


-- | Tries to fetch the previous page of a 'Pager'.  Returns
-- 'Nothing' whenever the current @Pager@ does not have a
-- 'pagerPrevious'.
fetchPreviousPage :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
                     Pager a -> FacebookT anyAuth m (Maybe (Pager a))
fetchPreviousPage = fetchHelper pagerPrevious


-- | (Internal) See 'fetchNextPage' and 'fetchPreviousPage'.
fetchHelper :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
               (Pager a -> Maybe String) -> Pager a -> FacebookT anyAuth m (Maybe (Pager a))
fetchHelper pagerRef pager =
  case pagerRef pager of
    Nothing  -> return Nothing
    Just url -> do
      req <- liftIO (H.parseUrl url)
      Just <$> (asJson =<< fbhttp req { H.redirectCount = 3 })


-- | Tries to fetch all next pages and returns a 'C.Source' with
-- all results.  The 'C.Source' will include the results from
-- this page as well.  Previous pages will not be considered.
-- Next pages will be fetched on-demand.
fetchAllNextPages ::
  (Monad m, C.MonadResource n, MonadBaseControl IO n, A.FromJSON a) =>
  Pager a -> FacebookT anyAuth m (C.Source n a)
fetchAllNextPages = fetchAllHelper pagerNext


-- | Tries to fetch all previous pages and returns a 'C.Source'
-- with all results.  The 'C.Source' will include the results
-- from this page as well.  Next pages will not be
-- considered.  Previous pages will be fetched on-demand.
fetchAllPreviousPages ::
  (Monad m, C.MonadResource n, MonadBaseControl IO n, A.FromJSON a) =>
  Pager a -> FacebookT anyAuth m (C.Source n a)
fetchAllPreviousPages = fetchAllHelper pagerPrevious


-- | (Internal) See 'fetchAllNextPages' and 'fetchAllPreviousPages'.
fetchAllHelper ::
  (Monad m, C.MonadResource n, MonadBaseControl IO n, A.FromJSON a) =>
  (Pager a -> Maybe String) -> Pager a -> FacebookT anyAuth m (C.Source n a)
fetchAllHelper pagerRef pager = do
  manager <- getManager
  let go (x:xs) mnext   = C.yield x >> go xs mnext
      go [] Nothing     = return ()
      go [] (Just next) = do
        req <- liftIO (H.parseUrl next)
        let get = fbhttpHelper manager req { H.redirectCount = 3 }
        start =<< asJson =<< lift get
      start p = go (pagerData p) $! pagerRef pager
  return (start pager)


----------------------------------------------------------------------


-- | Create an 'Argument' with a 'SimpleType'.  See the docs on
-- 'createAction' for an example.
(#=) :: SimpleType a => ByteString -> a -> Argument
p #= v = (p, encodeFbParam v)


-- | Class for data types that may be represented as a Facebook
-- simple type. (see
-- <https://developers.facebook.com/docs/opengraph/simpletypes/>).
class SimpleType a where
    encodeFbParam :: a -> B.ByteString

-- | Facebook's simple type @Boolean@.
instance SimpleType Bool where
    encodeFbParam b = if b then "1" else "0"

-- | Facebook's simple type @DateTime@ with only the date.
instance SimpleType TI.Day where
    encodeFbParam = B.pack . TI.formatTime defaultTimeLocale "%Y-%m-%d"
-- | Facebook's simple type @DateTime@.
instance SimpleType TI.UTCTime where
    encodeFbParam = B.pack . TI.formatTime defaultTimeLocale "%Y%m%dT%H%MZ"
-- | Facebook's simple type @DateTime@.
instance SimpleType TI.ZonedTime where
    encodeFbParam = encodeFbParam . TI.zonedTimeToUTC

-- @Enum@ doesn't make sense to support as a Haskell data type.

-- | Facebook's simple type @Float@ with less precision than supported.
instance SimpleType Float where
    encodeFbParam = showBS
-- | Facebook's simple type @Float@.
instance SimpleType Double where
    encodeFbParam = showBS

-- | Facebook's simple type @Integer@.
instance SimpleType Int where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int8 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word8 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int16 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word16 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int32 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word32 where
    encodeFbParam = showBS

-- | Facebook's simple type @String@.
instance SimpleType Text where
    encodeFbParam = TE.encodeUtf8
-- | Facebook's simple type @String@.
instance SimpleType ByteString where
    encodeFbParam = id

-- | An object's 'Id' code.
instance SimpleType Id where
    encodeFbParam = idCode

-- | A comma-separated list of simple types.  This definition
-- doesn't work everywhere, just for a few combinations that
-- Facebook uses (e.g. @[Int]@).  Also, encoding a list of lists
-- is the same as encoding the concatenation of all lists.  In
-- other words, this instance is here more for your convenience
-- than to make sure your code is correct.
instance SimpleType a => SimpleType [a] where
    encodeFbParam = B.concat . intersperse "," . map encodeFbParam

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show


----------------------------------------------------------------------


-- | Information about a place.  This is not a Graph Object,
-- instead it's just a field of a Object.  (Not to be confused
-- with the @Page@ object.)
data Place =
  Place { placeId       :: Id         -- ^ @Page@ ID.
        , placeName     :: Maybe Text -- ^ @Page@ name.
        , placeLocation :: Maybe Location
        }
  deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Place where
  parseJSON (A.Object v) =
    Place <$> v A..:  "id"
          <*> v A..:? "name"
          <*> v A..:? "location"
  parseJSON _ = mzero


-- | A geographical location.
data Location =
  Location { locationStreet  :: Maybe Text
           , locationCity    :: Maybe Text
           , locationState   :: Maybe Text
           , locationCountry :: Maybe Text
           , locationZip     :: Maybe Text
           , locationCoords  :: Maybe GeoCoordinates
           }
  deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Location where
  parseJSON obj@(A.Object v) =
    Location <$> v A..:? "street"
             <*> v A..:? "city"
             <*> v A..:? "state"
             <*> v A..:? "country"
             <*> v A..:? "zip"
             <*> A.parseJSON obj
  parseJSON _ = mzero


-- | Geographical coordinates.
data GeoCoordinates =
  GeoCoordinates { latitude  :: !Double
                 , longitude :: !Double
                 }
  deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON GeoCoordinates where
  parseJSON (A.Object v) =
    GeoCoordinates <$> v A..: "latitude"
                   <*> v A..: "longitude"
  parseJSON _ = mzero

instance SimpleType GeoCoordinates where
  encodeFbParam c =
    let obj  = A.object [ "latitude"  A..= latitude  c
                        , "longitude" A..= longitude c]
        toBS = TE.encodeUtf8 . TL.toStrict . TLB.toLazyText . AE.fromValue
    in toBS obj
