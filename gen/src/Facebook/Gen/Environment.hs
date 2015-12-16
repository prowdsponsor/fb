{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Facebook.Gen.Environment
where

import Control.Monad
import Control.Lens hiding (coerce)
import qualified Data.Map.Strict as Map
import Data.Vector hiding (map, length, head, tail, (++))
import qualified Data.Vector as V
import Data.Text hiding (map, length, head, tail)
import qualified Data.Text as T
import Data.Coerce
import Data.Maybe

import Facebook.Gen.Csv
import Facebook.Gen.Types

-- mapping from FB types to Haskell types
typesMap :: Map.Map Text Text
typesMap =
    Map.fromList [("string", "Text")
                , ("unsigned int32", "Word32")
                , ("float", "Float")
                , ("boolean", "Bool")
                , ("bool", "Bool")
                , ("datetime", "UTCTime") -- ???
                , ("numeric string", "Text")
                , ("integer", "Integer")
                , ("list<unsigned int32>", "Vector Word32")
                , ("list<string>", "Vector Text")
                , ("map<string, int32>", "Map.Map Text Int")
                , ("dictionary", "Map")] -- ???

type MapInEnv = Map.Map Entity (Map.Map InteractionMode (Vector FieldInfo))
newtype Env = Env MapInEnv deriving Show

envsToMaps :: Vector Env -> Vector MapInEnv
envsToMaps = coerce

buildEnv :: Vector (Vector CsvLine) -> Either Text Env
buildEnv csvs = do
    --let csvs' = join csvs :: Vector CsvLine
    let ignore = V.fromList ["rf_spec", "account_groups", "agency_client_declaration", "funding_source_details",
                             "owner_business", "business", "failed_delivery_checks"]
    let csvs' = V.filter (\(CsvLine _ mode _) -> mode == InteractionMode "Reading") (join csvs :: Vector CsvLine)
    let csvs'' = V.filter (\(CsvLine _ _ (FieldInfo name _ _ _ _ )) -> not $ V.elem name ignore) csvs'
    let envs = V.map buildEnvCsv csvs''
    let merged = merge envs
    --let uni = unify envs
    Right merged

merge :: V.Vector Env -> Env
-- this should be easier...
merge envs = go Map.empty $ envsToMaps envs
    where
        -- only Left if there is a name/type-clash... TODO
        go acc maps
            | V.null maps = Env acc
            | otherwise =
                let map = V.head maps
                in go (merge2 acc map) $ V.tail maps
        merge2 acc map
            | length (Map.keys map) == 1 -- since every line in the CSV file is turned into an Env
                = let key = head $ Map.keys map
                  in case acc ^.at key of
                        Nothing -> acc & at key ?~ (fromJust $ map ^.at key)
                        Just acc' -> let val = updateMap acc' $ fromJust $ map ^.at key
                                     in acc & at key ?~ val
            | otherwise = error "merge2"
        updateMap acc val
            | length (Map.keys val) == 1
                = let key = head $ Map.keys val
                  in case acc ^.at key of
                        Nothing -> acc & at key ?~ (fromJust $ val ^.at key)
                        Just acc' ->
                            let newElemV = fromJust $ val ^.at key
                                newElem  = V.head newElemV
                            in if V.elem newElem acc' -- duplicate field?
                                then error $ "Duplicate field: " ++ show newElem
                                else acc & at key ?~ (newElemV V.++ acc')
            | otherwise = error "updateMap"

buildEnvCsv :: CsvLine -> Env

buildEnvCsv (CsvLine (Entity ent) mode info) =
    let ent'  = (Entity $ T.concat $ splitOn " " ent)
        info' = insertHsType info
    in Env $ Map.insert ent' (Map.insert mode (V.singleton info') Map.empty) Map.empty

insertHsType :: FieldInfo -> FieldInfo
insertHsType fi =
    let fiType = type_ fi
        err = error $ "Could not find Haskell type for " ++ unpack fiType
                      ++ " for field " ++ unpack (name fi)
    in fi {type_ = Map.findWithDefault err fiType typesMap}

