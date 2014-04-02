{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Object.Action
   ( createAction
   , Action(..)
   ) where

import Control.Arrow (first)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function (on)
import Data.String (IsString(..))
import Data.Text (Text)

import qualified Control.Monad.Trans.Resource as R

import Facebook.Types
import Facebook.Monad
import Facebook.Graph


-- | Creates an Open Graph action on the user's timeline. Returns
-- the 'Id' of the newly created action.  For example:
--
-- > now <- liftIO getCurrentTime
-- > createAction "cook"
-- >              [ "recipe" #= "http://example.com/cookie.html"
-- >              , "when"   #= now ]
-- >              token
createAction :: (R.MonadResource m, MonadBaseControl IO m)  =>
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
  let post :: (R.MonadResource m, MonadBaseControl IO m)  => Text -> AccessToken anyKind -> FacebookT Auth m Id
      post prepath = postObject (prepath <> appName creds <> ":" <> action) query
  case mapptoken of
    Nothing       -> post "/me/" usertoken
    Just apptoken -> post ("/" <> idCode (accessTokenUserId usertoken) <> "/") apptoken


-- | An action of your app.  Please refer to Facebook's
-- documentation at
-- <https://developers.facebook.com/docs/opengraph/keyconcepts/#actions-objects>
-- to see how you can create actions.
--
-- This is a @newtype@ of 'Text' that supports only 'IsString'.
-- This means that to create an 'Action' you should use the
-- @OverloadedStrings@ language extension.  For example,
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > foo token = do
-- >   ...
-- >   createAction "cook" [...] token
newtype Action = Action { unAction :: Text }

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
