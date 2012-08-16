{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Facebook.OpenGraph
    ( createAction
    , Action(..)
    , createCheckin
    , fqlQuery
    , FQLResult(..)
    , (#=)
    , SimpleType(..)
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad (mzero)
import Data.ByteString.Char8 (ByteString)
import Data.Function (on)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
-- import Data.Typeable (Typeable, Typeable1)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32, Word)
import Data.String (IsString(..))
import System.Locale (defaultTimeLocale)

-- import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode as AE (fromValue)
import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit as C
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TI
-- import qualified Network.HTTP.Types as HT


import Facebook.Types
import Facebook.Monad
import Facebook.Base
import Facebook.Graph


-- | Creates an Open Graph action on the user's timeline. Returns
-- the 'Id' of the newly created action.  For example:
--
-- > now <- liftIO getCurrentTime
-- > createAction "cook"
-- >              [ "recipe" #= "http://example.com/cookie.html"
-- >              , "when"   #= now ]
-- >              token
createAction :: (C.MonadResource m, MonadBaseControl IO m)  =>
                Action     -- ^ Action kind to be created.
             -> [Argument] -- ^ Arguments of the action.
             -> Maybe AppAccessToken
                -- ^ Optional app access token (optional with
                -- respect to this library, since you can't make
                -- this mandatory by changing the settings of
                -- your action on Facebook).
             -> UserAccessToken -- ^ Required user access token.
             -> FacebookT Auth m Id
createAction (Action action) query mapptoken usertoken = do
  creds <- getCreds
  let post :: (C.MonadResource m, MonadBaseControl IO m)  => ByteString -> AccessToken anyKind -> FacebookT Auth m Id
      post prepath = postObject (prepath <> appName creds <> ":" <> action) query
  case mapptoken of
    Nothing       -> post "/me/" usertoken
    Just apptoken -> post ("/" <> accessTokenUserId usertoken <> "/") apptoken


-- | An action of your app.  Please refer to Facebook's
-- documentation at
-- <https://developers.facebook.com/docs/opengraph/keyconcepts/#actions-objects>
-- to see how you can create actions.
--
-- This is a @newtype@ of 'ByteString' that supports only 'IsString'.
-- This means that to create an 'Action' you should use the
-- @OverloadedStrings@ language extension.  For example,
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > foo token = do
-- >   ...
-- >   createAction "cook" [...] token
newtype Action = Action { unAction :: ByteString }

instance Show Action where
    show = show . unAction

-- | Since 0.7.1
instance Eq Action where
    (==) = (==) `on` unAction
    (/=) = (/=) `on` unAction

-- | Since 0.7.1
instance Ord Action where
    compare = compare `on` unAction
    (<=) = (<=) `on` unAction
    (<)  = (<)  `on` unAction
    (>=) = (>=) `on` unAction
    (>)  = (>)  `on` unAction

-- | Since 0.7.1
instance Read Action where
    readsPrec = (fmap (first Action) .) . readsPrec

instance IsString Action where
    fromString = Action . fromString


-- | Creates a 'check-in' and returns its id. Place and
-- coordinates are both required by Facebook.
createCheckin :: (C.MonadResource m, MonadBaseControl IO m)  =>
                 Id               -- ^ Place Id
              -> (Double, Double) -- ^ (Latitude, Longitude)
              -> [Argument]       -- ^ Other arguments of the action.
              -> UserAccessToken  -- ^ Required user access token.
              -> FacebookT Auth m Id
createCheckin pid (lat,lon) args usertoken = do
  let coords = ("coordinates", toBS $ A.object ["latitude" A..= lat, "longitude" A..= lon])
      body = ("place" #= pid) : coords : args
      toBS = TE.encodeUtf8 . TL.toStrict . toLazyText . AE.fromValue
  postObject "me/checkins" body usertoken


-- | Query the Facebook Graph using FQL.  You may want to use
-- 'FQLResult' when parsing the returned JSON.
fqlQuery :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>
             Text                        -- ^ FQL Query
          -> Maybe (AccessToken anyKind) -- ^ Optional access token
          -> FacebookT anyAuth m a
fqlQuery fql mtoken =
  runResourceInFb $ do
    let query = ["q" #= fql]
    asJson =<< fbhttp =<< fbreq "/fql" mtoken query


-- | Parses an FQL query result.  FQL query results are always of the form
--
-- @
--   { "data": [ret1, ret2, ...] }
-- @
--
-- This @newtype@ unwraps the array from the @"data"@ field
-- automatically for you, so you may write something like:
--
-- @
--   FQLResult [...] <- fqlQuery ...
-- @
newtype FQLResult a = FQLResult [a] deriving (Eq, Ord, Show, Read)

instance A.FromJSON a => A.FromJSON (FQLResult a) where
  parseJSON (A.Object v) = FQLResult <$> (v A..: "data")
  parseJSON _ = mzero


-- | Create an 'Argument' with a 'SimpleType'.  See the docs on
-- 'createAction' for an example.
(#=) :: SimpleType a => ByteString -> a -> Argument
p #= v = (p, encodeFbParam v)



-- | Class for data types that may be represented as a Facebook
-- simple type. (see
-- <https://developers.facebook.com/docs/opengraph/simpletypes/>).
class SimpleType a where
    encodeFbParam :: a -> B.ByteString

-- | Facebook's simple type @Boolean@.
instance SimpleType Bool where
    encodeFbParam b = if b then "1" else "0"

-- | Facebook's simple type @DateTime@ with only the date.
instance SimpleType TI.Day where
    encodeFbParam = B.pack . TI.formatTime defaultTimeLocale "%Y-%m-%d"
-- | Facebook's simple type @DateTime@.
instance SimpleType TI.UTCTime where
    encodeFbParam = B.pack . TI.formatTime defaultTimeLocale "%Y%m%dT%H%MZ"
-- | Facebook's simple type @DateTime@.
instance SimpleType TI.ZonedTime where
    encodeFbParam = encodeFbParam . TI.zonedTimeToUTC

-- @Enum@ doesn't make sense to support as a Haskell data type.

-- | Facebook's simple type @Float@ with less precision than supported.
instance SimpleType Float where
    encodeFbParam = showBS
-- | Facebook's simple type @Float@.
instance SimpleType Double where
    encodeFbParam = showBS

-- | Facebook's simple type @Integer@.
instance SimpleType Int where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int8 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word8 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int16 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word16 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int32 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word32 where
    encodeFbParam = showBS

-- | Facebook's simple type @String@.
instance SimpleType Text where
    encodeFbParam = TE.encodeUtf8

-- | An object's 'Id' code.
instance SimpleType Id where
    encodeFbParam = idCode

-- | A comma-separated list of simple types.  This definition
-- doesn't work everywhere, just for a few combinations that
-- Facebook uses (e.g. @[Int]@).  Also, encoding a list of lists
-- is the same as encoding the concatenation of all lists.  In
-- other words, this instance is here more for your convenience
-- than to make sure your code is correct.
instance SimpleType a => SimpleType [a] where
    encodeFbParam = B.concat . intersperse "," . map encodeFbParam

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show
