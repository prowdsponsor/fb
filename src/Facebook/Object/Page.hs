{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , OverloadedStrings
           #-}
module Facebook.Object.Page
       ( Page(..)
       , getPage
       , searchPages
       ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:), (.:?))
import Data.ByteString.Char8 (ByteString)
import qualified Data.Aeson as A
import qualified Data.Conduit as C
import Data.Text (Text)
import Data.Typeable (Typeable)

import Facebook.Graph
import Facebook.Monad
import Facebook.Types

-- | A Facebook page (see
-- <https://developers.facebook.com/docs/reference/api/page/>).
--
-- /NOTE:/ Does not yet support all fields. Please file an issue if
-- you need any other fields.
data Page = Page { pageId                :: ByteString
                 , pageName              :: Maybe Text
                 , pageLink              :: Maybe ByteString
                 , pageCategory          :: Maybe Text
                 , pageIsPublished       :: Maybe Bool
                 , pageCanPost           :: Maybe Bool
                 , pageLikes             :: Maybe Integer
                 , pageLocation          :: Maybe Location
                 , pagePhone             :: Maybe Text
                 , pageCheckins          :: Maybe Integer
                 , pagePicture           :: Maybe ByteString
                 , pageWebsite           :: Maybe ByteString
                 , pageTalkingAboutCount :: Maybe Integer
                 } deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Page where
  parseJSON (A.Object v) =
    Page <$> v .: "id"
         <*> v .:? "name"
         <*> v .:? "link"
         <*> v .:? "category"
         <*> v .:? "is_published"
         <*> v .:? "can_post"
         <*> v .:? "likes"
         <*> v .:? "location"
         <*> v .:? "phone"
         <*> v .:? "checkin"
         <*> v .:? "picture"
         <*> v .:? "website"
         <*> v .:? "talking_about_count"
  parseJSON _ = mzero


-- | Get a page using its ID. The user access token is optional.
getPage :: (C.MonadResource m, MonadBaseControl IO m)
        => ByteString            -- ^ Page ID
        -> [Argument]            -- ^ Arguments to be passed to Facebook
        -> Maybe UserAccessToken -- ^ Optional user access token
        -> FacebookT anyAuth m Page
getPage id_ = getObject $ "/" <> id_


-- | Search pages by keyword. The user access token is optional.
searchPages :: (C.MonadResource m, MonadBaseControl IO m)
            => ByteString            -- ^ Keyword to search for
            -> [Argument]            -- ^ Arguments to pass to Facebook
            -> Maybe UserAccessToken -- ^ Optional user access token
            -> FacebookT anyAuth m (Pager Page)
searchPages = searchObjects "page"
