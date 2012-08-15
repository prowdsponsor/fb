{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Graph
    ( getObject
    , postObject
    , Id(..)
    , searchObjects
    , SearchResultPage(..)
    ) where


import Control.Applicative
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad (mzero)
-- import Data.ByteString.Char8 (ByteString)
-- import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Types (Ascii)

-- import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Conduit as C
-- import qualified Data.Text as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import Facebook.Types
import Facebook.Monad
import Facebook.Base


-- | Make a raw @GET@ request to Facebook's Graph API.  Returns a
-- raw JSON 'A.Value'.
getObject :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
             Ascii          -- ^ Path (should begin with a slash @\/@)
          -> [Argument]     -- ^ Arguments to be passed to Facebook
          -> Maybe (AccessToken anyKind) -- ^ Optional access token
          -> FacebookT anyAuth m a
getObject path query mtoken =
  runResourceInFb $
    asJson =<< fbhttp =<< fbreq path mtoken query


-- | Make a raw @POST@ request to Facebook's Graph API.  Returns
-- a raw JSON 'A.Value'.
postObject :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
              Ascii               -- ^ Path (should begin with a slash @\/@)
           -> [Argument]          -- ^ Arguments to be passed to Facebook
           -> AccessToken anyKind -- ^ Access token
           -> FacebookT Auth m a
postObject path query token =
  runResourceInFb $ do
    req <- fbreq path (Just token) query
    asJson =<< fbhttp req { H.method = HT.methodPost }


-- | The identification code of an object.
newtype Id = Id { idCode :: Ascii }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Id where
    parseJSON (A.Object v) = Id <$> v A..: "id"
    parseJSON other        = Id <$> A.parseJSON other


-- | Make a raw @GET@ request to the /search endpoint of Facebook’s
-- Graph API.  Returns a raw JSON 'A.Value'.
searchObjects :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a)
              => Ascii                 -- ^ A Facebook object type to search for
              -> Ascii                 -- ^ The keyword to search for
              -> [Argument]            -- ^ Additional arguments to pass
              -> Maybe UserAccessToken -- ^ Optional access token
              -> FacebookT anyAuth m (SearchResultPage a)
searchObjects objectType keyword query = getObject "/search" query'
  where query' = ("q", keyword) : ("type", objectType) : query


-- | The result object for searchObjects. The type parameter is
-- expected to be an instance of A.FromJSON, but nothing has been done
-- to assure that Facebook will return the type expected.
data SearchResultPage a = SearchResultPage
                          { searchResults :: [a]
                          , searchPage :: Maybe Pager
                          } deriving (Eq, Ord, Show, Read, Typeable)

instance (A.FromJSON a) => A.FromJSON (SearchResultPage a) where
  parseJSON (A.Object v) = SearchResultPage
                           <$> v A..: "data"
                           <*> v A..:? "paging"
  parseJSON _ = mzero


-- | Simply wraps potential links for previous and next pages within a
-- search result set.  TODO: Replace with a type that encapsulates the
-- paging requests instead of just their URLs?
data Pager = Pager { previousPage :: Maybe Ascii
                   , nextPage :: Maybe Ascii
                   } deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Pager where
  parseJSON (A.Object v) = Pager
                           <$> v A..:? "previous"
                           <*> v A..:? "next"
  parseJSON _ = mzero
