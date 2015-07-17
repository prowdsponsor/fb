{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdImage where

import Facebook.Object.Marketing.Utility
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics
import Data.Typeable (Typeable)

data AdImage = AdImage
  { ai_hash            :: Text
  , ai_url             :: Text
  , ai_creatives       :: Text
  , ai_id              :: Text
  , ai_width           :: Text
  , ai_height          :: Text
  , ai_original_width  :: Text
  , ai_original_height :: Text
  } deriving (Eq, Ord, Show, Read, Typeable, Generic)

instance A.FromJSON AdImage where
  parseJSON = parseJSONWithPrefix "ai_"

instance A.ToJSON AdImage where
  toJSON = toJSONWithPrefix "ai_"
