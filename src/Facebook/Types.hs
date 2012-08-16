{-# LANGUAGE DeriveDataTypeable, GADTs, StandaloneDeriving #-}
module Facebook.Types
    ( Credentials(..)
    , AccessToken(..)
    , UserAccessToken
    , AppAccessToken
    , AccessTokenData
    , UserId
    , accessTokenData
    , accessTokenExpires
    , accessTokenUserId
    , UserKind
    , AppKind
    , Argument
    , (<>)
    ) where

import Data.ByteString (ByteString)
import Data.Monoid (Monoid, mappend)
import Data.Time (UTCTime)
import Data.Typeable (Typeable, Typeable1)


-- | Credentials that you get for your app when you register on
-- Facebook.
data Credentials =
    Credentials { appName   :: ByteString -- ^ Your application name (e.g. for OpenGraph calls).
                , appId     :: ByteString -- ^ Your application ID.
                , appSecret :: ByteString -- ^ Your application secret key.
                }
    deriving (Eq, Ord, Show, Read, Typeable)


-- | An access token.  While you can make some API calls without
-- an access token, many require an access token and some will
-- give you more information with an appropriate access token.
--
-- There are two kinds of access tokens:
--
-- [User access token] An access token obtained after an user
-- accepts your application.  Let's you access more information
-- about that user and act on their behalf (depending on which
-- permissions you've asked for).
--
-- [App access token] An access token that allows you to take
-- administrative actions for your application.
--
-- These two kinds of access tokens are distinguished by the
-- phantom type on 'AccessToken', which can be 'UserKind' or
-- 'AppKind'.
data AccessToken kind where
    UserAccessToken :: UserId -> AccessTokenData -> UTCTime -> AccessToken UserKind
    AppAccessToken  :: AccessTokenData -> AccessToken AppKind

-- | Type synonym for @'AccessToken' 'UserKind'@.
type UserAccessToken = AccessToken UserKind

-- | Type synonym for @'AccessToken' 'AppKind'@.
type AppAccessToken = AccessToken AppKind


deriving instance Eq   (AccessToken kind)
deriving instance Ord  (AccessToken kind)
deriving instance Show (AccessToken kind)
deriving instance Typeable1 AccessToken

-- | The access token data that is passed to Facebook's API
-- calls.
type AccessTokenData = ByteString

-- | A Facebook user id such as @1008905713901@.
type UserId = ByteString

-- | Get the access token data.
accessTokenData :: AccessToken anyKind -> AccessTokenData
accessTokenData (UserAccessToken _ d _) = d
accessTokenData (AppAccessToken d)      = d

-- | Expire time of an access token.  It may never expire, in
-- which case it will be @Nothing@.
accessTokenExpires :: AccessToken anyKind -> Maybe UTCTime
accessTokenExpires (UserAccessToken _ _ expt) = Just expt
accessTokenExpires (AppAccessToken _)         = Nothing

-- | Get the user ID of an user access token.
accessTokenUserId :: UserAccessToken -> UserId
accessTokenUserId (UserAccessToken uid _ _) = uid

-- | Phantom type used mark an 'AccessToken' as an user access
-- token.
data UserKind deriving (Typeable)

-- | Phantom type used mark an 'AccessToken' as an app access
-- token.
data AppKind deriving (Typeable)


-- | An argument given to an API call.
type Argument = (ByteString, ByteString)


-- | Synonym for 'mappend'.
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
