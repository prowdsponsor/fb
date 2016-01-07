{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Demographies where

import Data.Text (Text, unpack, pack)
import Data.Aeson
--import Facebook.Object.Marketing.Types

-- | Demographics and events
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#demographics

data Gender = Male | Female | Any deriving (Show, Eq)

instance ToJSON Gender where
  toJSON Male   = toJSON ([1] :: [Int])
  toJSON Female = toJSON ([2] :: [Int])
  toJSON Any   = Null

data TargetUserAge = TargetUserAge { getAge :: Int } deriving (Show, Eq, Ord)

mkAge :: Int -> TargetUserAge
mkAge x | (x >= 13) && (x <= 65) = TargetUserAge x
mkAge _ = error "Out of facebook target age."

data Demography = Demography
  { genders :: Gender
  , age_min :: Maybe TargetUserAge
  , age_max :: Maybe TargetUserAge
  } deriving (Show, Eq)
