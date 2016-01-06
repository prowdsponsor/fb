{-# LANGUAGE DeriveGeneric #-}
module Facebook.Gen.Csv
where

import Control.Applicative (pure)
import Control.Monad (mzero)
import Data.Csv
import Data.Text hiding (length)
import qualified Data.Text as T
import Control.Applicative
import qualified Data.Vector as V

import Facebook.Gen.Types

data CsvLine = CsvLine {
      entity  :: Entity
    , interactionMode :: InteractionMode
    , fieldInfo :: FieldInfo
    } deriving Show

instance FromRecord CsvLine where
    parseRecord v
        | V.length v == 7 =
            let ent = Entity <$> v .! 0
                mode = v .! 1
                fn = v .! 2
                fntype = v .! 3
                desc = v .! 4
                req = v .! 5
                rsp = v .! 6
                fieldInfo = FieldInfo <$> fn <*> fntype <*> desc <*> req <*> rsp
            in CsvLine <$> ent <*> mode <*> fieldInfo
        | otherwise = mzero
