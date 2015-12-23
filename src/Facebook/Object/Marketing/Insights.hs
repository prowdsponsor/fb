{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.Insights where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import GHC.Generics
import Data.Time
import Data.Typeable (Typeable)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager

import Facebook.Object.Marketing.Types hiding (Id)
import Facebook.Object.Marketing.Utility

data Action a = Action { action_action_type :: Text,
                         action_value :: a }
                deriving (Eq, Show, Typeable, Generic)

instance A.FromJSON a => A.FromJSON (Action a) where
  parseJSON = parseJSONWithPrefix "action_"

instance A.ToJSON a => A.ToJSON (Action a) where
  toJSON = toJSONWithPrefix "action_"


data Insights = Insights
  { ins_actions :: [Action Int],
    ins_unique_actions :: [Action Double],
    ins_cost_per_action_type :: [Action Double],
    ins_cost_per_unique_action_type :: [Action Double],
    ins_call_to_action_clicks :: Int,
    ins_unique_clicks :: Int,
    ins_cpm :: Double,
    ins_ctr :: Double,
    ins_cpp :: Double,
    ins_unique_ctr :: Double,
    ins_unique_impressions:: Int,
    ins_reach :: Int,
    ins_spend:: Double } deriving (Eq, Show, Typeable, Generic)

instance A.FromJSON Insights where
  parseJSON = parseJSONWithPrefix "ins_"

instance A.ToJSON Insights where
  toJSON = toJSONWithPrefix "ins_"


getInsights :: (R.MonadResource m, MonadBaseControl IO m)  =>
                     Id
                  -> [Argument]
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager (WithJSON Insights))
getInsights (Id id_) query tok = getObject ("/v2.5/" <> id_ <> "/insights") query (Just tok)
