{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdImage where


import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics

import Facebook.Graph
import Facebook.Monad
import Facebook.Types
import Facebook.Pager
import Facebook.Object.Marketing.Utility
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.AdAccount

data AdImage = AdImage
  { ai_hash            :: Text
  , ai_url             :: Text
  , ai_creatives       :: Text
  , ai_id              :: Text
  , ai_width           :: Text
  , ai_height          :: Text
  , ai_original_width  :: Text
  , ai_original_height :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance A.FromJSON AdImage where
  parseJSON = parseJSONWithPrefix "ai_"

instance A.ToJSON AdImage where
  toJSON = toJSONWithPrefix "ai_"

data ImageUpload = ImageFile FilePath
                 | Zip FilePath

uploadImage :: (R.MonadResource m, MonadBaseControl IO m)  =>
                 AdAccountId  -- ^ Ad Account ID.
              -> UserAccessToken  -- ^ Required user access token.
              -> FacebookT Auth m FBMObjectCreated
uploadImage aid usertoken = do
  postObject ("/" <> toFbText aid  <>"/adimages") [] usertoken
