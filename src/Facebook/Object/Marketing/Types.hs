
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.Types where

import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import qualified Data.Aeson as A

newtype Money = Money {getMoney :: Text} deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON Money
instance A.ToJSON Money
