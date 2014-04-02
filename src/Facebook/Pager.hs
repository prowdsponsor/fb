{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Pager
    ( Pager(..)
    , fetchNextPage
    , fetchPreviousPage
    , fetchAllNextPages
    , fetchAllPreviousPages
    ) where


import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.Typeable (Typeable)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Conduit as C
import qualified Network.HTTP.Conduit as H


import Facebook.Base
import Facebook.Monad


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
fetchNextPage :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
                 Pager a -> FacebookT anyAuth m (Maybe (Pager a))
fetchNextPage = fetchHelper pagerNext


-- | Tries to fetch the previous page of a 'Pager'.  Returns
-- 'Nothing' whenever the current @Pager@ does not have a
-- 'pagerPrevious'.
fetchPreviousPage :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
                     Pager a -> FacebookT anyAuth m (Maybe (Pager a))
fetchPreviousPage = fetchHelper pagerPrevious


-- | (Internal) See 'fetchNextPage' and 'fetchPreviousPage'.
fetchHelper :: (R.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
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
  (Monad m, MonadResourceBase n, A.FromJSON a) =>
  Pager a -> FacebookT anyAuth m (C.Source n a)
fetchAllNextPages = fetchAllHelper pagerNext


-- | Tries to fetch all previous pages and returns a 'C.Source'
-- with all results.  The 'C.Source' will include the results
-- from this page as well.  Next pages will not be
-- considered.  Previous pages will be fetched on-demand.
fetchAllPreviousPages ::
  (Monad m, MonadResourceBase n, A.FromJSON a) =>
  Pager a -> FacebookT anyAuth m (C.Source n a)
fetchAllPreviousPages = fetchAllHelper pagerPrevious


-- | (Internal) See 'fetchAllNextPages' and 'fetchAllPreviousPages'.
fetchAllHelper ::
  (Monad m, MonadResourceBase n, A.FromJSON a) =>
  (Pager a -> Maybe String) -> Pager a -> FacebookT anyAuth m (C.Source n a)
fetchAllHelper pagerRef pager = do
  manager <- getManager
  let go (x:xs) mnext   = C.yield x >> go xs mnext
      go [] Nothing     = return ()
      go [] (Just next) = do
        req <- liftIO (H.parseUrl next)
        let get = fbhttpHelper manager req { H.redirectCount = 3 }
        start =<< lift (R.runResourceT $ asJsonHelper =<< get)
      start p = go (pagerData p) $! pagerRef p
  return (start pager)
