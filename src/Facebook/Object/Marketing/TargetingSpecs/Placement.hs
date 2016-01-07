{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Placement where

import Data.Text (Text, unpack, pack)
import Data.Aeson
--import Facebook.Object.Marketing.Types

-- | Placement
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#location

data PlacementOption = Desktopfeed
                     | RightColumn
                     | MobileFeed
                     | MobileExternal
                     | Home
                     deriving (Show, Eq)
