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
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Typeable (Typeable)

import Facebook.Graph
import Facebook.Monad
import Facebook.Types
import Facebook.Pager

-- | A Facebook page (see
-- <https://developers.facebook.com/docs/reference/api/page/>).
--
-- /NOTE:/ Does not yet support all fields. Please file an issue if
-- you need any other fields.
data Page = Page { pageId                :: Id
                 , pageName              :: Maybe Text
                 , pageLink              :: Maybe Text
                 , pageCategory          :: Maybe Text
                 , pageIsPublished       :: Maybe Bool
                 , pageCanPost           :: Maybe Bool
                 , pageLikes             :: Maybe Integer
                 , pageLocation          :: Maybe Location
                 , pagePhone             :: Maybe Text
                 , pageCheckins          :: Maybe Integer
                 , pagePicture           :: Maybe Text
                 , pageWebsite           :: Maybe Text
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
getPage :: (R.MonadResource m, MonadBaseControl IO m)
        => Id                    -- ^ Page ID
        -> [Argument]            -- ^ Arguments to be passed to Facebook
        -> Maybe UserAccessToken -- ^ Optional user access token
        -> FacebookT anyAuth m Page
getPage id_ = getObject $ "/" <> idCode id_


-- | Search pages by keyword. The user access token is optional.
searchPages :: (R.MonadResource m, MonadBaseControl IO m)
            => Text                  -- ^ Keyword to search for
            -> [Argument]            -- ^ Arguments to pass to Facebook
            -> Maybe UserAccessToken -- ^ Optional user access token
            -> FacebookT anyAuth m (Pager Page)
searchPages = searchObjects "page"
