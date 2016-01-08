{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Data.Aeson.Types
import Data.Default
--import Facebook.Object.Marketing.Types
import GHC.Generics (Generic)
import Facebook.Records
import Facebook.Object.Marketing.TargetingSpecs.Demographies
import Facebook.Object.Marketing.TargetingSpecs.Location
import Facebook.Object.Marketing.TargetingSpecs.Mobile
import Facebook.Object.Marketing.TargetingSpecs.Placement
import qualified Data.ByteString.Lazy as BL

data TargetingSpecs = TargetingSpecs
  { --exclusions :: Maybe Value
   geo_locations :: TargetLocation
  --, mobile_targeting :: Maybe MobileTargeting -- ^ Please note that the fields of this record will be at the top level
  --, page_types :: Maybe [PlacementOption]
  } deriving (Show, Eq, Generic)

instance ToJSON TargetingSpecs where -- FIXME
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
  --toJSON (TargetingSpecs exclusions geo mobile placement) =
  --    let Object m = toJSON mobile
  --        fieldsExcludingMobile = undefined
  --    in undefined m fieldsExcludingMobile

instance FromJSON TargetingSpecs where
    parseJSON (Object v) = undefined

instance ToBS TargetingSpecs where
    toBS a = toBS $ toJSON a

instance ToBS TargetLocation where
    toBS = toBS . toJSON

instance ToBS Value where
    toBS = BL.toStrict . encode
