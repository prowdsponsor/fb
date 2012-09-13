{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Graph
    ( getObject
    , postObject
    , Id(..)
    , searchObjects
    , Pager(..)
    , fetchNextPage
    , fetchPreviousPage
    , fetchAllNextPages
    , fetchAllPreviousPages
    ) where


import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad (mzero)
import Data.ByteString.Char8 (ByteString)
-- import Data.Text (Text)
import Data.Typeable (Typeable)

-- import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Conduit as C
-- import qualified Data.Text as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import Facebook.Types
import Facebook.Monad
import Facebook.Base


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


-- | The identification code of an object.
newtype Id = Id { idCode :: ByteString }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Id where
    parseJSON (A.Object v) = Id <$> v A..: "id"
    parseJSON other        = Id <$> A.parseJSON other


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
fetchAllNextPages ::
  (Monad m, C.MonadResource n, MonadBaseControl IO n, A.FromJSON a) =>
  Pager a -> FacebookT anyAuth m (C.Source n a)
fetchAllNextPages = fetchAllHelper pagerNext


-- | Tries to fetch all previous pages and returns a 'C.Source'
-- with all results.  The 'C.Source' will include the results
-- from this page as well.  Next pages will not be
-- considered.
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
