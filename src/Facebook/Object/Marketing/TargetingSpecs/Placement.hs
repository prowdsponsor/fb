{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Placement where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Char

-- | Placement
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#location

data PlacementOption = Desktopfeed
                     | RightColumn
                     | MobileFeed
                     | MobileExternal
                     | Home
                     | InstagramStream
                     deriving (Show, Eq, Generic)
instance ToJSON PlacementOption where
    toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance FromJSON PlacementOption where
    parseJSON (String "desktopfeed") = pure Desktopfeed
    parseJSON (String "rightcolumn") = pure RightColumn
    parseJSON (String "mobilefeed") = pure MobileFeed
    parseJSON (String "mobileexternal") = pure MobileExternal
    parseJSON (String "home") = pure Home
    parseJSON (String "instagramstream") = pure InstagramStream



