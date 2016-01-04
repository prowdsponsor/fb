module Facebook.Gen.CodeGenStrTypes where

import Data.Text

-- include this file in the generated Types.hs
oldTypesImport :: Text
oldTypesImport =
    "import Data.Text (Text, pack, unpack)\n\
    \import Data.Typeable (Typeable)\n\
    \import GHC.Generics\n\
    \import Data.Aeson\n\
    \import Data.Aeson.Types\n\
    \import Control.Applicative\n\
    \import Control.Monad\n\
    \import qualified Data.ByteString as BS\n\
    \import qualified Data.ByteString.Lazy as BSL\n\
    \import qualified Data.Text.Encoding as TE\n\
    \import Facebook.Object.Marketing.Utility hiding (toBS)\n\
    \import Text.Read (readMaybe)\n"

newTypes :: Text
newTypes =  -- does this nesting work for JSON???
    "data ConfiguredCampaignStatus = ACTIVE_ | PAUSED_ | DELETED_ | ARCHIVED_ deriving (Show, Generic)\n\
    \instance ToJSON ConfiguredCampaignStatus where\n\
    \\ttoJSON ACTIVE_ = String \"ACTIVE\"\n\
    \\ttoJSON PAUSED_ = String \"PAUSED\"\n\
    \\ttoJSON DELETED_ = String \"DELETED\"\n\
    \\ttoJSON ARCHIVED_ = String \"ARCHIVED\"\n\
    \instance FromJSON ConfiguredCampaignStatus where\n\
    \\tparseJSON (String \"ACTIVE\") = pure ACTIVE_\n\
    \\tparseJSON (String \"PAUSED\") = pure PAUSED_\n\
    \\tparseJSON (String \"DELETED\") = pure DELETED_\n\
    \\tparseJSON (String \"ARCHIVED\") = pure ARCHIVED_\n\
    \data EffectiveCampaignStatus = ACTIVE | PAUSED | DELETED | ARCHIVED | PENDING_REVIEW | DISAPPROVED | PREAPPROVED | PENDING_BILLING_INFO | CAMPAIGN_PAUSED | ADSET_PAUSED deriving (Show, Generic)\n\
    \instance FromJSON EffectiveCampaignStatus\n\
    \instance ToJSON EffectiveCampaignStatus\n"

oldTypes :: Text
oldTypes =
    "newtype Money = Money {getMoneyInPennies :: Int} deriving (Eq, Ord, Show, Read, Typeable, Generic, Num)\n\n\

    \instance FromJSON Money where\n\
      \\tparseJSON (String val) =\n\
      \\t\tcase readMaybe $ unpack val of\n\
            \\t\t\tJust x -> return $ Money x\n\
            \\t\t\tNothing -> fail $ \"Money parser string: \"++show (unpack val)\n\
                \\t\twhere\n\
                  \\t\t\ttextToBSL :: Text -> BSL.ByteString\n\
                  \\t\t\ttextToBSL = BSL.fromStrict . TE.encodeUtf8\n\
      \\tparseJSON n@(Number _) = do\n\
          \\t\tx <- parseJSON n\n\
          \\t\treturn $ Money x\n\
      \\tparseJSON v = fail $ \"Money parser value: \"++show v\n\n\

    \instance ToJSON Money where\n\
      \\ttoJSON (Money cents) = String $ pack $ show cents\n\n\

    \data FbNumeric = FbStringNumeric Text\n\
                   \\t\t| FbIntegerNumeric Int\n\
                                 \\tderiving (Show, Read, Generic)\n\

    \instance FromJSON FbNumeric\n\
    \instance ToJSON FbNumeric\n\n\

    \data FBMObjectCreated = FBMObjectCreated\n\
      \\t{ oc_id :: Text\n\
      \\t, oc_success :: Bool\n\
      \\t} deriving (Show, Generic)\n\

    \instance FromJSON FBMObjectCreated where\n\
      \\tparseJSON = parseJSONWithPrefix \"oc_\"\n\n\

    \class ToFbText a where\n\
      \\ttoFbText :: a -> Text\n"
