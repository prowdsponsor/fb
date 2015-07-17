module Facebook.Object.Marketing.Utility where

import Data.Aeson
import Data.Aeson.Types
import Data.List
import GHC.Generics

dropString :: String -> String -> String
dropString pre s = case stripPrefix pre s of
  Nothing      -> s
  Just dropped -> dropped

dropOption pre = defaultOptions
  { fieldLabelModifier = dropString pre
  , omitNothingFields = True
  }
parseJSONWithPrefix pre = genericParseJSON (dropOption pre)
toJSONWithPrefix pre = genericToJSON (dropOption pre)


camelOption = defaultOptions {fieldLabelModifier = camelTo '_'}
parseJSONCamel val = genericParseJSON camelOption val
toJSONCamel x = genericToJSON camelOption x
