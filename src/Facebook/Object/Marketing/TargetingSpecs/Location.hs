{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Location where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Data.Aeson.Types
import Facebook.Records
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import Facebook.Graph

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
  --, regions :: Maybe [Text]
  --, cities :: Maybe [Text]
  --, zips :: Maybe [Text]
  --, custom_locations :: Maybe [Text]
  --, geo_markets :: Maybe [Text]
  --, location_types :: Maybe LocationTypes
  } deriving (Show, Eq, Generic)

instance ToJSON TargetLocation where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON TargetLocation where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }
