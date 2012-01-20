module Facebook.Types
    ( Credentials(..)
    , AccessToken(..)
    , AccessTokenData
    , accessTokenData
    , accessTokenExpires
    , User
    , App
    , (<>)
    ) where

import Data.Monoid (Monoid, mappend)
import Data.Time (UTCTime)
import Data.Typeable (Typeable, Typeable1)
import Network.HTTP.Types (Ascii)


-- | Credentials that you get for your app when you register on
-- Facebook.
data Credentials =
    Credentials { appName   :: Ascii -- ^ Your application name (e.g. for OpenGraph calls).
                , appId     :: Ascii -- ^ Your application ID.
                , appSecret :: Ascii -- ^ Your application secret key.
                }
    deriving (Eq, Ord, Show, Typeable)


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
-- These access tokens are distinguished by the phantom type on
-- 'AccessToken', which can be 'User' or 'App'.
data AccessToken kind where
    UserAccessToken :: AccessTokenData -> UTCTime -> AccessToken User
    AppAccessToken  :: AccessTokenData -> AccessToken App

deriving instance Eq   (AccessToken kind)
deriving instance Ord  (AccessToken kind)
deriving instance Show (AccessToken kind)
deriving instance Typeable1 AccessToken

-- | The access token data that is passed to Facebook's API
-- calls.
type AccessTokenData = Ascii

-- | Get the access token data.
accessTokenData :: AccessToken kind -> AccessTokenData
accessTokenData (UserAccessToken d _) = d
accessTokenData (AppAccessToken d)    = d

-- | Expire time of an access token.  It may never expire, in
-- which case it will be @Nothing@.
accessTokenExpires :: AccessToken kind -> Maybe UTCTime
accessTokenExpires (UserAccessToken _ expt) = Just expt
accessTokenExpires (AppAccessToken _)       = Nothing

-- | Phantom type used mark an 'AccessToken' as an user access
-- token.
data User deriving (Typeable)

-- | Phantom type used mark an 'AccessToken' as an app access
-- token.
data App deriving (Typeable)


-- | Synonym for 'mappend'.
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
