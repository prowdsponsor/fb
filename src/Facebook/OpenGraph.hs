module Facebook.OpenGraph
    ( createAction
    , Action(..)
    ) where

-- import Control.Applicative
-- import Control.Monad (mzero)
-- import Data.ByteString.Char8 (ByteString)
-- import Data.Text (Text)
-- import Data.Typeable (Typeable, Typeable1)
import Data.String (IsString(..))
import Network.HTTP.Types (Ascii)

-- import qualified Control.Exception.Lifted as E
-- import qualified Data.Aeson as A
import qualified Data.Conduit as C
-- import qualified Data.Text as T
import qualified Network.HTTP.Types as HT


import Facebook.Types
import Facebook.Monad
-- import Facebook.Base
import Facebook.Graph


-- | Creates an Open Graph action on the user's timeline. Returns
-- the 'Id' of the newly created action.
createAction :: C.ResourceIO m =>
                Action               -- ^ Action kind to be created.
             -> HT.SimpleQuery       -- ^ Arguments of the action.
             -> AccessToken User
             -> FacebookT Auth m Id
createAction (Action action) query token = do
  creds <- getCreds
  postObject ("/me/" <> appName creds <> ":" <> action) query token


-- | An action of your app.  Please refer to Facebook's
-- documentation at
-- <https://developers.facebook.com/docs/opengraph/keyconcepts/#actions-objects>
-- to see how you can create actions.
--
-- This is a @newtype@ of 'Ascii' that supports only 'IsString'.
-- This means that to create an 'Action' you should use the
-- @OverloadedStrings@ language extension.  For example,
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > foo token = do
-- >   ...
-- >   createAction "cook" [...] token
newtype Action = Action { unAction :: Ascii }

instance Show Action where
    show = show . unAction

instance IsString Action where
    fromString = Action . fromString


