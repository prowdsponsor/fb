{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings, ConstraintKinds #-}

module Facebook.Object.Marketing.AdAccountGroupPlayground where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics hiding ((:*:))

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import Data.Aeson.Types

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Records
import Facebook.Object.Marketing.AdUser (AdUser)

import Facebook.Object.Marketing.Utility

import qualified Data.Vector as V

-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/marketing-api/adaccountgroup/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.

data AdAccountGroupStatus = Active
                          | Deleted
                          deriving Show
instance A.FromJSON AdAccountGroupStatus where
  parseJSON val = do
    intCode <- A.parseJSON val :: Parser Int
    case intCode of
      1 -> return Active
      2 -> return Deleted
      _ -> mzero
  parseJSON _ = mzero

instance A.ToJSON AdAccountGroupStatus where
  toJSON Active = Number 1
  toJSON Deleted = Number 2

data AACG_Id = AACG_Id
instance Field AACG_Id where
    type FieldValue AACG_Id = Id
    fieldName _ = "id"
    fieldLabel = AACG_Id

data AACG_Name = AACG_Name
instance Field AACG_Name where
    type FieldValue AACG_Name = Text
    fieldName _ = "name"
    fieldLabel = AACG_Name

data AACG_Status = AACG_Status
instance Field AACG_Status where
    type FieldValue AACG_Status = AdAccountGroupStatus
    fieldName _ = "status"
    fieldLabel = AACG_Status

data AACG_Users = AACG_Users
instance Field AACG_Users where
    type FieldValue AACG_Users = V.Vector AdAccountGroupUsers
    fieldName _ = "users"
    fieldLabel = AACG_Users

data AACG_ACC_ID = AACG_ACC_ID
instance Field AACG_ACC_ID where
    type FieldValue AACG_ACC_ID = Id
    fieldName _ = "account_group_id"
    fieldLabel = AACG_ACC_ID

data AdAccountGroupUsers = AdAccountGroupUsers {
      role  :: Int
    , uid   :: Id
    } deriving (Show, Generic)
instance ToJSON AdAccountGroupUsers
instance FromJSON AdAccountGroupUsers

data AACG_Accounts = AACG_Accounts
instance Field AACG_Accounts where
    type FieldValue AACG_Accounts = V.Vector AdAccountGroupAdAccounts
    fieldName _ = "accounts"
    fieldLabel = AACG_Accounts

data AdAccountGroupAdAccounts = AdAccountGroupAdAccounts {
      account_id  :: Int
    , status  :: AdAccountGroupStatus
    } deriving (Show, Generic)
instance ToJSON AdAccountGroupAdAccounts
instance FromJSON AdAccountGroupAdAccounts

type AdAccountGroupC r =
    (Has AACG_Accounts r,
     Has AACG_Users r,
     Has AACG_Name r,
     Has AACG_Status r,
     Has AACG_ACC_ID r)

type AdAccountGroupResult =
    AACG_Accounts :*: AACG_Status :*: AACG_Name :*: AACG_Users :*: AACG_ACC_ID :*: Nil

testGroup :: AdAccountGroupResult
testGroup = (AACG_Accounts, V.fromList [AdAccountGroupAdAccounts 1234 Active]) :*: (AACG_Status, Deleted)
                :*: (AACG_Name, "TEST_GROUP") :*: (AACG_Users, V.fromList [AdAccountGroupUsers 567 "ID"])
                :*: (AACG_ACC_ID, Id "testID") :*: Nil

