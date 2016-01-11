{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Demographies where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Data.Maybe
import GHC.Generics (Generic)

-- | Demographics and events
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#demographics

data Gender = Male | Female | Any deriving (Show, Eq)

instance ToJSON Gender where
  toJSON Male   = toJSON ([1] :: [Int])
  toJSON Female = toJSON ([2] :: [Int])
  toJSON Any   = Null

newtype TargetUserAge = TargetUserAge Int deriving (Show, Eq, Ord, Generic)
instance ToJSON TargetUserAge

mkAge :: Int -> TargetUserAge
mkAge x | (x >= 13) && (x <= 65) = TargetUserAge x
mkAge _ = error "Out of facebook target age."

data Demography = Demography
  { genders :: Gender
  , age_min :: Maybe TargetUserAge
  , age_max :: Maybe TargetUserAge
  } deriving (Show, Eq, Generic)

instance ToJSON Demography where
    toJSON = demoToJSON

demoToJSON :: Demography -> Value
demoToJSON (Demography g amin amax) =
    let g' = Just $ "genders" .= toJSON g
        ageJson val str = case val of
                            Nothing -> Nothing
                            Just age -> Just $ str .= toJSON age
        amax' = ageJson amax "age_max"
        amin' = ageJson amin "age_min"
    in object $ catMaybes $ [g', amin', amax']
