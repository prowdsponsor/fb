{-# LANGUAGE DeriveDataTypeable, GADTs, StandaloneDeriving #-}
module Facebook.Types
    ( Credentials(..)
    , appIdBS
    , appSecretBS
    , AccessToken(..)
    , UserAccessToken
    , AppAccessToken
    , AccessTokenData
    , Id(..)
    , UserId
    , accessTokenData
    , accessTokenExpires
    , accessTokenUserId
    , UserKind
    , AppKind
    , Argument
    , (<>)
    , FbUTCTime(..)
    ) where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid (Monoid, mappend)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime, parseTime)
import Data.Typeable (Typeable, Typeable1)
import System.Locale (defaultTimeLocale)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLBI


-- | Credentials that you get for your app when you register on
-- Facebook.
data Credentials =
    Credentials { appName   :: Text -- ^ Your application name (e.g. for Open Graph calls).
                , appId     :: Text -- ^ Your application ID.
                , appSecret :: Text -- ^ Your application secret key.
                }
    deriving (Eq, Ord, Show, Read, Typeable)

-- | 'appId' for 'ByteString'.
appIdBS :: Credentials -> ByteString
appIdBS = TE.encodeUtf8 . appId

-- | 'appSecret' for 'ByteString'.
appSecretBS :: Credentials -> ByteString
appSecretBS = TE.encodeUtf8 . appSecret


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
type AccessTokenData = Text

-- | The identification code of an object.
newtype Id = Id { idCode :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, IsString)

instance A.FromJSON Id where
    parseJSON (A.Object v) = v A..: "id"
    parseJSON (A.String s) = pure $ Id s
    parseJSON (A.Number d) = pure $ Id $ from $ floor d
      where from i = TL.toStrict $ TLB.toLazyText $ TLBI.decimal (i :: Int64)
    parseJSON o = fail $ "Can't parse Facebook.Id from " ++ show o


-- | A Facebook user ID such as @1008905713901@.
type UserId = Id


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


-- | (Internal) @newtype@ for 'UTCTime' that follows Facebook's
-- conventions of JSON parsing.  While @aeson@ expects a format
-- of @%FT%T%Q@, Facebook gives time values formatted as
-- @%FT%T%z@.
newtype FbUTCTime = FbUTCTime { unFbUTCTime :: UTCTime }

instance A.FromJSON FbUTCTime where
  parseJSON (A.String t) =
    case parseTime defaultTimeLocale "%FT%T%z" (T.unpack t) of
      Just d -> return (FbUTCTime d)
      _      -> fail $ "could not parse FbUTCTime string " ++ show t
  parseJSON _ = fail "could not parse FbUTCTime from something which is not a string"
