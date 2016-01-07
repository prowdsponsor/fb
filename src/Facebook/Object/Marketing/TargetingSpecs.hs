{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Data.Default
--import Facebook.Object.Marketing.Types
import Facebook.Records
import Facebook.Object.Marketing.TargetingSpecs.Demographies
import Facebook.Object.Marketing.TargetingSpecs.Location
import Facebook.Object.Marketing.TargetingSpecs.Mobile
import Facebook.Object.Marketing.TargetingSpecs.Placement

data TargetingSpecs = TargetingSpecs
  { exclusions :: Value
  , geo_location :: Maybe TargetLocation
  , mobile_targeting :: Maybe MobileTargeting -- ^ Please note that the fields of this record will be at the top level
  , page_types :: [PlacementOption]
  } deriving (Show, Eq)

instance ToJSON TargetingSpecs where -- FIXME
  toJSON (TargetingSpecs exclusions geo mobile placement) =
      let Object m = toJSON mobile
          fieldsExcludingMobile = undefined
      in undefined m fieldsExcludingMobile

instance FromJSON TargetingSpecs where
    parseJSON (Object v) = undefined

instance ToBS TargetingSpecs
