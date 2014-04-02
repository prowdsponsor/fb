{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Object.FriendList
    ( FriendList(..)
    , FriendListType(..)
    , getUserFriendLists
    , getFriendListMembers
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson ((.:))
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A


import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.User

-- | A friend list for a 'User'.
data FriendList =
    FriendList { friendListId    :: Id
               , friendListName  :: Text
               , friendListType  :: FriendListType
               }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON FriendList where
    parseJSON (A.Object v) =
      FriendList <$> v .: "id"
                 <*> v .: "name"
                 <*> v .: "list_type"
    parseJSON _ = mzero

data FriendListType = CloseFriendsList | AcquaintancesList | RestrictedList
    | UserCreatedList | EducationList | WorkList | CurrentCityList | FamilyList
    deriving (Eq, Ord, Show, Read, Enum, Typeable)

instance A.FromJSON FriendListType where
    parseJSON (A.String "close_friends") = return CloseFriendsList
    parseJSON (A.String "acquaintances") = return AcquaintancesList
    parseJSON (A.String "restricted")    = return RestrictedList
    parseJSON (A.String "user_created")  = return UserCreatedList
    parseJSON (A.String "education")     = return EducationList
    parseJSON (A.String "work")          = return WorkList
    parseJSON (A.String "current_city")  = return CurrentCityList
    parseJSON (A.String "family")        = return FamilyList
    parseJSON _                          = mzero

instance A.ToJSON FriendListType where
    toJSON = A.toJSON . toText
        where
          toText :: FriendListType -> Text
          toText CloseFriendsList  = "close_friends"
          toText AcquaintancesList = "aquaintances"
          toText RestrictedList    = "restricted"
          toText UserCreatedList   = "user_created"
          toText EducationList     = "education"
          toText WorkList          = "work"
          toText CurrentCityList   = "current_city"
          toText FamilyList        = "family"

-- close_friends, acquaintances, restricted, user_created, education, work, current_city, family

-- | Get the friend lists of the given user.
getUserFriendLists ::
     (R.MonadResource m, MonadBaseControl IO m) =>
     UserId          -- ^ User ID or @\"me\"@.
  -> [Argument]      -- ^ Arguments to be passed to Facebook.
  -> UserAccessToken -- ^ User access token.
  -> FacebookT anyAuth m (Pager FriendList)
getUserFriendLists id_ query token =
  getObject ("/" <> idCode id_ <> "/friendlists") query (Just token)

-- | Get the members of a friend list.
getFriendListMembers ::
     (R.MonadResource m, MonadBaseControl IO m) =>
     Id              -- ^ List ID.
  -> [Argument]      -- ^ Arguments to be passed to Facebook.
  -> UserAccessToken -- ^ User access token.
  -> FacebookT anyAuth m (Pager Friend)
getFriendListMembers id_ query token =
  getObject ("/" <> idCode id_ <> "/members") query (Just token)
