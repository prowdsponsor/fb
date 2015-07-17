{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}
module Facebook.Object.Marketing.ImageCrop where

import GHC.Generics
import Data.Typeable (Typeable)

data CropKey = CropKey { ckValue :: (Int, Int)
                       , ckAspectRatio :: Double
                       } deriving (Eq, Ord, Show, Read, Typeable, Generic)

getCropKey :: Placement -> CropKey
getCropKey NewsFeedCarousel = CropKey (100,100) 1
getCropKey NewsFeedRHS = CropKey (191,100) 1.91
getCropKey NewsFeedSmall = CropKey (400,150) 2.67
getCropKey NewsFeedLarge = CropKey (600,360) 1.67

data CropValue = CropValue (Int,Int) (Int,Int)
                 deriving (Eq, Ord, Show, Read, Typeable, Generic)

data Placement = NewsFeedCarousel | NewsFeedRHS |
                 NewsFeedSmall | NewsFeedLarge
                 deriving (Eq, Ord, Show, Read, Typeable, Generic)

data ImageCrop = ImageCrop CropKey CropValue
                deriving (Eq, Ord, Show, Read, Typeable, Generic)
