{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdCampaign where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text, pack, stripPrefix)
import Data.Typeable (Typeable)
import GHC.Generics

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Time -- Check if FbUTCTime is used

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.Checkin
import Facebook.Object.Marketing.AdAccount (AdAccount, AdAccountId)

import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.Utility

data AdCamapign = AdCampaign
  { ac_id :: Id
  , account_id :: AdAccountId
  , buying_type :: Text
  , campaign_group_status :: Text
  , can_use_spend_cap :: Bool
  , created_time :: UTCTime
  , name :: Text
  , objective :: Integer
  , start_time :: UTCTime
  , stop_time :: UTCTime
  , topline_id :: Maybe Text
  , updated_time :: UTCTime
  , spend_cap :: Money
  } deriving Show
