{-# LANGUAGE DeriveGeneric #-}
module Facebook.Gen.Csv
where

import Control.Monad
import Data.Csv
import Data.Text hiding (length)
import qualified Data.Text as T

import Facebook.Gen.Types

data CsvLine = CsvLine {
      entity  :: Entity
    , interactionMode :: InteractionMode
    , fieldInfo :: FieldInfo
    } deriving Show

instance FromRecord CsvLine where
    parseRecord v
        | length v == 7 =
            let ent = Entity <$> v .! 0
                mode = InteractionMode <$> v .! 1
                fn = v .! 2
                fntype = v .! 3
                desc = v .! 4
                req = v .! 5
                rsp = v .! 6
                fieldInfo = FieldInfo <$> fn <*> fntype <*> desc <*> req <*> rsp
            in CsvLine <$> ent <*> mode <*> fieldInfo
        | otherwise = mzero

-- turns "acc_id" into "AccId"
-- TODO: simplify... this looks horrible
fieldNameToADTName :: Text -> Text
fieldNameToADTName str
    | T.null $ dropUnderscore str = str
    | otherwise =
        let str' = dropUnderscore str
            first = charToUpper $ T.head str'
            camel = toCamelCase "" $ T.tail str'
        in first `append` camel
    where
        dropUnderscore = T.dropWhile (=='_')
        charToUpper = toUpper . singleton
        toCamelCase acc str
            | T.null str = acc
            | otherwise =
                let (a, b) = breakOn "_" str
                    b' = dropUnderscore b
                    first =  charToUpper $ T.head b'
                in if T.null b'
                    then toCamelCase (acc `append` a) b'
                    else toCamelCase (acc `append` a `append` first) $ T.tail b'
