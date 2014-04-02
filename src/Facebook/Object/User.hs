{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Object.User
    ( User(..)
    , Gender(..)
    , getUser
    , searchUsers
    , getUserCheckins
    , Friend(..)
    , getUserFriends
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:), (.:?))
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A


import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.Checkin


-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/reference/api/user/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.
data User =
    User { userId         :: UserId
         , userName       :: Maybe Text
         , userFirstName  :: Maybe Text
         , userMiddleName :: Maybe Text
         , userLastName   :: Maybe Text
         , userGender     :: Maybe Gender
         , userLocale     :: Maybe Text
         , userUsername   :: Maybe Text
         , userVerified   :: Maybe Bool
         , userEmail      :: Maybe Text
         , userLocation   :: Maybe Place
         }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON User where
    parseJSON (A.Object v) =
      User <$> v .:  "id"
           <*> v .:? "name"
           <*> v .:? "first_name"
           <*> v .:? "middle_name"
           <*> v .:? "last_name"
           <*> v .:? "gender"
           <*> v .:? "locale"
           <*> v .:? "username"
           <*> v .:? "verified"
           <*> v .:? "email"
           <*> v .:? "location"
    parseJSON _ = mzero


-- | An user's gender.
data Gender = Male | Female deriving (Eq, Ord, Show, Read, Enum, Typeable)

instance A.FromJSON Gender where
    parseJSON (A.String "male")   = return Male
    parseJSON (A.String "female") = return Female
    parseJSON _                   = mzero

instance A.ToJSON Gender where
    toJSON = A.toJSON . toText
        where
          toText :: Gender -> Text
          toText Male   = "male"
          toText Female = "female"


-- | Get an user using his user ID.  The user access token is
-- optional, but when provided more information can be returned
-- back by Facebook.  The user ID may be @\"me\"@, in which
-- case you must provide an user access token and information
-- about the token's owner is given.
getUser :: (R.MonadResource m, MonadBaseControl IO m) =>
           UserId         -- ^ User ID or @\"me\"@.
        -> [Argument]     -- ^ Arguments to be passed to Facebook.
        -> Maybe UserAccessToken -- ^ Optional user access token.
        -> FacebookT anyAuth m User
getUser id_ query mtoken = getObject ("/" <> idCode id_) query mtoken


-- | Search users by keyword.
searchUsers :: (R.MonadResource m, MonadBaseControl IO m)
            => Text
            -> [Argument]
            -> Maybe UserAccessToken
            -> FacebookT anyAuth m (Pager User)
searchUsers = searchObjects "user"


-- | Get a list of check-ins made by a given user.
getUserCheckins ::
     (R.MonadResource m, MonadBaseControl IO m) =>
     UserId          -- ^ User ID or @\"me\"@.
  -> [Argument]      -- ^ Arguments to be passed to Facebook.
  -> UserAccessToken -- ^ User access token.
  -> FacebookT anyAuth m (Pager Checkin)
getUserCheckins id_ query token =
  getObject ("/" <> idCode id_ <> "/checkins") query (Just token)


-- | A friend connection of a 'User'.
data Friend =
    Friend { friendId   :: UserId
           , friendName :: Text
           }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON Friend where
    parseJSON (A.Object v) =
      Friend <$> v .: "id"
             <*> v .: "name"
    parseJSON _ = mzero


-- | Get the list of friends of the given user.
getUserFriends ::
     (R.MonadResource m, MonadBaseControl IO m) =>
     UserId          -- ^ User ID or @\"me\"@.
  -> [Argument]      -- ^ Arguments to be passed to Facebook.
  -> UserAccessToken -- ^ User access token.
  -> FacebookT anyAuth m (Pager Friend)
getUserFriends id_ query token =
  getObject ("/" <> idCode id_ <> "/friends") query (Just token)
