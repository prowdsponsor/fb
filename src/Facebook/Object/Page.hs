{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , OverloadedStrings
           #-}
module Facebook.Object.Page
       ( Page(..)
       , getPage
       ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Conduit as C
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Types (Ascii)

import Facebook.Graph
import Facebook.Monad
import Facebook.Object.User
import Facebook.Types

-- | A Facebook page (see
-- <https://developers.facebook.com/docs/reference/api/page/>).
--
-- /NOTE:/ Does not yet support all fields. Please file an issue if
-- you need any other fields.
data Page = Page { pageId                :: Ascii
                 , pageName              :: Maybe Text
                 , pageLink              :: Maybe Ascii
                 , pageCategory          :: Maybe Text
                 , pageIsPublished       :: Maybe Bool
                 , pageCanPost           :: Maybe Bool
                 , pageLikes             :: Maybe Integer
                 , pageLocation          :: Maybe UserLocation
                 , pagePhone             :: Maybe Text
                 , pageCheckins          :: Maybe Integer
                 , pagePicture           :: Maybe Ascii
                 , pageWebsite           :: Maybe Ascii
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


-- | Get a page using its ID.  The user access token is optional, but
-- when provided more information can be returned back by Facebook.
getPage :: (C.MonadResource m, MonadBaseControl IO m) =>
           Ascii                 -- ^ Page ID
        -> [Argument]            -- ^ Arguments to be passed to Facebook.
        -> Maybe UserAccessToken -- ^ Optional user access token.
        -> FacebookT anyAuth m Page
getPage id_ = getObject ("/" <> id_)
