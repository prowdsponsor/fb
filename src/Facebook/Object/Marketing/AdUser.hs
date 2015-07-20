{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdUser where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A


import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager

data Permission =  AcccountAdmin | AdManagerRead | AdManagerWrite |
                   BillingRead |BillingWrite | Reports
                   deriving (Show, Eq, Ord, Read, Typeable)

instance A.FromJSON Permission where
  parseJSON (A.Number 1) = return AcccountAdmin
  parseJSON (A.Number 2) = return AdManagerRead
  parseJSON (A.Number 3) = return AdManagerWrite
  parseJSON (A.Number 4) = return BillingRead
  parseJSON (A.Number 5) = return BillingWrite
  -- 6 skipped
  parseJSON (A.Number 7) = return Reports
  -- 8 skipped
  parseJSON (A.Number 9) = fail "Only applies to directly managed accounts"
  parseJSON (A.Number 10) = fail "Only applies to directly managed accounts"
  parseJSON _ = mzero

instance A.ToJSON Permission where
  toJSON AcccountAdmin = A.Number 1
  toJSON AdManagerRead = A.Number 2
  toJSON AdManagerWrite = A.Number 3
  toJSON BillingRead = A.Number 4
  toJSON BillingWrite = A.Number 5
  toJSON Reports = A.Number 7

data Role = Administrator | Advertiser | Analyst | DirectSales
            deriving (Show, Eq, Ord, Read, Typeable)

instance A.FromJSON Role where
  parseJSON (A.Number 1001) = return Administrator
  parseJSON (A.Number 1002) = return Advertiser
  parseJSON (A.Number 1003) = return Analyst
  parseJSON (A.Number 1004) = return DirectSales

  parseJSON _ = mzero

instance A.ToJSON Role where
  toJSON Administrator = A.Number 1001
  toJSON Advertiser = A.Number 1002
  toJSON Analyst = A.Number 1003
  toJSON DirectSales = A.Number 1004

-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/marketing-api/aduser/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.
data AdUser =
    AdUser { au_id          :: UserId
           , au_permissions :: Maybe Permission
           , au_role        :: Maybe Role
           }
    deriving (Eq, Show, Read, Typeable, Generic)

instance A.FromJSON AdUser
instance A.ToJSON AdUser


addAdUserToAccount :: AdUser -> m ()
addAdUserToAccount = undefined

deleteAdUser :: UserId -> m ()
deleteAdUser = undefined

successMsg :: A.Value
successMsg = A.object ["sucesss" A..= True]
