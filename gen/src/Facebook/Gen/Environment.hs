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
type ModeFieldInfoMap = Map.Map InteractionMode (Vector FieldInfo)
type EntityModeMap = Map.Map Entity ModeFieldInfoMap
newtype Env = Env EntityModeMap deriving Show

envsToMaps :: Vector Env -> Vector EntityModeMap
envsToMaps = coerce

buildEnv :: Vector (Vector CsvLine) -> Either Text Env
buildEnv csvs = do
    --let csvs' = join csvs :: Vector CsvLine
    let ignore = V.fromList $ ["rf_spec", "account_groups", "agency_client_declaration", "funding_source_details",
                               "owner_business", "business", "failed_delivery_checks"] -- Ad Account
                               ++ ["effective_status", "adlabels", "configured_status"] -- Campaign
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
        merge2 acc entToModeMap
            | length (Map.keys entToModeMap) == 1 -- since every line in the CSV file is turned into an Env
                = let key = head $ Map.keys entToModeMap
                  in case acc ^.at key of -- is current entity elem of final env?
                        Nothing -> acc & at key ?~ (fromJust $ entToModeMap ^.at key)
                        Just acc' -> let modeMap = updateModeMap acc' $ fromJust $ entToModeMap ^.at key
                                     in acc & at key ?~ modeMap
            | otherwise = error "merge2"

updateModeMap :: ModeFieldInfoMap -> ModeFieldInfoMap -> ModeFieldInfoMap
updateModeMap acc modeMap
    | length (Map.keys modeMap) == 1
        = let key = head $ Map.keys modeMap
          in case acc ^.at key of
                Nothing  -> acc & at key ?~ (fromJust $ modeMap ^.at key) -- mode is not in map
                Just fis -> acc & at key ?~ (updateFieldInfo fis $ fromJust $ modeMap ^.at key)
    | otherwise = error "updateModeMap"

-- updateFieldInfo :: V n Fi -> V 1 Fi -> V (n+1) Fi
updateFieldInfo :: Vector FieldInfo -> Vector FieldInfo -> Vector FieldInfo
updateFieldInfo fis fiV =
    let fi = V.head fiV
    in case V.find (==fi) fis of -- duplicate field?
        Nothing -> fiV V.++ fis
        Just a -> error $ "Duplicate fields: " ++ show fi ++ " and " ++ show a -- unify
            --if type_ a == type_ fi
            --    then ...
            --    else error $ "Duplicate fields with different types: " ++ show fi ++ " and " ++ show a -- unify

buildEnvCsv :: CsvLine -> Env
buildEnvCsv (CsvLine (Entity ent) mode info) =
    let ent'  = (Entity $ T.concat $ splitOn " " ent)
        info' = insertHsType info (Entity ent) mode
    in Env $ Map.insert ent' (Map.insert mode (V.singleton info') Map.empty) Map.empty

insertHsType :: FieldInfo -> Entity -> InteractionMode -> FieldInfo
insertHsType fi ent mode =
    let fiType = type_ fi
        err = error $ "Could not find Haskell type for " ++ unpack fiType
                      ++ " for field " ++ unpack (name fi) ++ " in " ++ show ent
                      ++ ", mode " ++ show mode
    in fi {type_ = Map.findWithDefault err fiType typesMap}
