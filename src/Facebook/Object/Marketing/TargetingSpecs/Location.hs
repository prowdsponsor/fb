{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Location where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Facebook.Object.Marketing.Types
import GHC.Generics

-- | Location
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#location

data LocationTypes = Recent
                   | Home
                   | TravelIn
                     deriving (Eq, Show, Generic)

instance FromJSON LocationTypes
instance ToJSON LocationTypes

data TargetLocation = TargetLocation
  { countries :: [Text]
  , regions :: [Text]
  , cities :: [Text]
  , zips :: [Text]
  , custom_locations :: [Text]
  , geo_markets :: [Text]
  , location_types :: LocationTypes
  } deriving (Show, Eq, Generic)

instance FromJSON TargetLocation
instance ToJSON TargetLocation
