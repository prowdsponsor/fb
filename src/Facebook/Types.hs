{-# LANGUAGE CPP, DeriveDataTypeable, GADTs, StandaloneDeriving #-}
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

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid (Monoid, mappend)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime, parseTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (Typeable, Typeable1)
import Data.Data (Data)
#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
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
    deriving (Eq, Ord, Show, Read, Typeable, Data)

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

instance A.ToJSON Id where
    toJSON (Id t) = A.String t


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


----------------------------------------------------------------------


-- | /Since 0.14.9./ Not a Facebook JSON format, but a custom @fb@
-- format for convenience if you need to serialize access tokens.
instance A.ToJSON (AccessToken kind) where
  toJSON (UserAccessToken uid data_ expires) =
    A.object
      [ "kind"    A..= ("user" :: Text)
      , "id"      A..= uid
      , "token"   A..= data_
      , "expires" A..= expires ]
  toJSON (AppAccessToken data_) =
    A.object
      [ "kind"    A..= ("app" :: Text)
      , "token"   A..= data_ ]


-- | (Internal) Since the user of 'parseJSON' is going to choose
-- via its @kind@ whether a 'UserAccessToken' or an
-- 'AppAccessToken' is wanted, we need this type class to
-- implement 'FromJSON'.
class ParseAccessToken kind where
  parseTokenJSON :: A.Object -> A.Parser (AccessToken kind)

instance ParseAccessToken UserKind where
  parseTokenJSON v =
    checkKind v "user" $
      UserAccessToken <$> v A..: "id"
                      <*> v A..: "token"
                      <*> v A..: "expires"

instance ParseAccessToken AppKind where
  parseTokenJSON v =
    checkKind v "app" $
      AppAccessToken <$> v A..: "token"

-- | (Internal) Used to implement 'parseTokenJSON'.
checkKind :: A.Object -> Text -> A.Parser a -> A.Parser a
checkKind v kind ok = do
  kind' <- v A..: "kind"
  if kind == kind'
    then ok
    else fail $ "Expected access token kind " <> show kind <>
                " but found " <> show kind' <> "."

-- | /Since 0.14.9./ Parses the format that 'ToJSON' produces.
-- Note that you need to statically decide whether you want to
-- parse a user access token or an app access token.
instance ParseAccessToken kind => A.FromJSON (AccessToken kind) where
  parseJSON (A.Object v) = parseTokenJSON v
  parseJSON _            = mzero


----------------------------------------------------------------------


-- | @newtype@ for 'UTCTime' that follows Facebook's
-- conventions of JSON parsing.
--
--  * As a string, while @aeson@ expects a format of @%FT%T%Q@,
--    Facebook gives time values formatted as @%FT%T%z@.
--
--  * As a number, 'FbUTCTime' accepts a number of seconds since
--    the Unix epoch.
newtype FbUTCTime = FbUTCTime { unFbUTCTime :: UTCTime }
  deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON FbUTCTime where
  parseJSON (A.String t) =
    case parseTime defaultTimeLocale "%FT%T%z" (T.unpack t) of
      Just d -> return (FbUTCTime d)
      _      -> fail $ "could not parse FbUTCTime string " ++ show t
  parseJSON (A.Number n) =
    return $ FbUTCTime $ posixSecondsToUTCTime $ fromInteger $ floor n
  parseJSON _ = fail "could not parse FbUTCTime from something which is not a string or number"
