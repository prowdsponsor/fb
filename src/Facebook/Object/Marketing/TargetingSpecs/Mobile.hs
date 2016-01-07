{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Mobile where

import GHC.Generics
import Data.Text (Text, unpack, pack)
import Data.Aeson
--import Facebook.Object.Marketing.Types

-- | Mobile
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#mobile

data WirelessCarrier = Wifi deriving (Eq, Show, Generic)

instance ToJSON WirelessCarrier

data MobileTargeting = MobileTargetting
   { user_os :: MobileOS
   , user_device :: [Text] -- search/?type=adTargetingCategory&class=user_device
   , wireless_carrier :: WirelessCarrier
   } deriving (Show, Eq, Generic)

instance ToJSON MobileTargeting

mobOSToText (IOS IOS_Any) = "iOS"
mobOSToText (IOS version) = error "Not implemented"
mobOSToText (Android Android_Any) = "Android"
mobOSToText (Android version) =  error "Not implemented"

instance ToJSON MobileOS where
  toJSON = String . mobOSToText


data IOSVersion  = IOS_2 | IOS_3 | IOS_4 | IOS_4_3 | IOS_5 |
                   IOS_6 | IOS_7 | IOS_8 | IOS_Any
                   deriving (Eq, Show, Ord)

data AndroidVersion = Android_Any
                    | Android_2_0
                    | Android_2_1
                    | Android_2_2
                    | Android_2_3
                    | Android_3_0
                    | Android_3_1
                    | Android_3_2
                    | Android_4_0
                    | Android_4_1
                    | Android_4_2
                    | Android_4_3
                    | Android_4_4
                    deriving (Eq, Show, Ord)

data MobileOS = IOS IOSVersion
              | Android AndroidVersion
              deriving (Eq, Show)
