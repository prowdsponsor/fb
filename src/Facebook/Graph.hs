{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Graph
    ( getObject
    , postObject
    , deleteObject
    , searchObjects
    , (#=)
    , SimpleType(..)
    , Place(..)
    , Location(..)
    , GeoCoordinates(..)
    , Tag(..)
    ) where


import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import System.Locale (defaultTimeLocale)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE (fromValue)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Time as TI
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import Facebook.Auth
import Facebook.Base
import Facebook.Monad
import Facebook.Types
import Facebook.Pager


-- | Make a raw @GET@ request to Facebook's Graph API.
getObject :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
             Text           -- ^ Path (should begin with a slash @\/@)
          -> [Argument]     -- ^ Arguments to be passed to Facebook
          -> Maybe (AccessToken anyKind) -- ^ Optional access token
          -> FacebookT anyAuth m a
getObject path query mtoken =
  runResourceInFb $
    asJson =<< fbhttp =<< fbreq path mtoken query


-- | Make a raw @POST@ request to Facebook's Graph API.
postObject :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
              Text                -- ^ Path (should begin with a slash @\/@)
           -> [Argument]          -- ^ Arguments to be passed to Facebook
           -> AccessToken anyKind -- ^ Access token
           -> FacebookT Auth m a
postObject = methodObject HT.methodPost


-- | Make a raw @DELETE@ request to Facebook's Graph API.
deleteObject :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
                Text                -- ^ Path (should begin with a slash @\/@)
             -> [Argument]          -- ^ Arguments to be passed to Facebook
             -> AccessToken anyKind -- ^ Access token
             -> FacebookT Auth m a
deleteObject = methodObject HT.methodDelete


-- | Helper function used by 'postObject' and 'deleteObject'.
methodObject :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
                HT.Method
             -> Text                -- ^ Path (should begin with a slash @\/@)
             -> [Argument]          -- ^ Arguments to be passed to Facebook
             -> AccessToken anyKind -- ^ Access token
             -> FacebookT Auth m a
methodObject method path query token =
  runResourceInFb $ do
    req <- fbreq path (Just token) query
    asJson =<< fbhttp req { H.method = method }


-- | Make a raw @GET@ request to the /search endpoint of Facebook’s
-- Graph API.  Returns a raw JSON 'A.Value'.
searchObjects :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a)
              => Text                  -- ^ A Facebook object type to search for
              -> Text                  -- ^ The keyword to search for
              -> [Argument]            -- ^ Additional arguments to pass
              -> Maybe UserAccessToken -- ^ Optional access token
              -> FacebookT anyAuth m (Pager a)
searchObjects objectType keyword query = getObject "/search" query'
  where query' = ("q" #= keyword) : ("type" #= objectType) : query


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
-- | Facebook's simple type @Integer@.
instance SimpleType Int64 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word64 where
    encodeFbParam = showBS

-- | Facebook's simple type @String@.
instance SimpleType Text where
    encodeFbParam = TE.encodeUtf8
-- | Facebook's simple type @String@.
instance SimpleType ByteString where
    encodeFbParam = id

-- | An object's 'Id' code.
instance SimpleType Id where
    encodeFbParam = TE.encodeUtf8 . idCode

-- | 'Permission' is a @newtype@ of 'Text'
instance SimpleType Permission where
    encodeFbParam = encodeFbParam . unPermission

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


-- | A tag (i.e. \"I'll /tag/ you on my post\").
data Tag =
  Tag { tagId   :: Id   -- ^ Who is tagged.
      , tagName :: Text -- ^ Name of the tagged person.
      }
  deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Tag where
  parseJSON (A.Object v) =
    Tag <$> v A..: "id"
        <*> v A..: "name"
  parseJSON _ = mzero
