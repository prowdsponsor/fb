{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Facebook.Gen.Environment
where

import Control.Monad
import Control.Lens hiding (coerce)
import qualified Data.Map.Strict as Map
import Data.Vector hiding (map, length, head, tail, (++), concat)
import qualified Data.Vector as V
import Data.Text hiding (map, length, head, tail, concat)
import qualified Data.Text as T
import Data.Coerce
import Data.Maybe
import qualified Prelude as P
import Prelude

import Facebook.Gen.Csv
import Facebook.Gen.Types

import Debug.Trace

-- mapping from FB types to Haskell types
typesMap :: Map.Map Text Text
typesMap =
    Map.fromList [("string", "Text")
                , ("unsigned int32", "Word32")
                , ("int32", "Int")
                , ("int", "Integer")
                , ("float", "Float")
                , ("boolean", "Bool")
                , ("bool", "Bool")
                , ("datetime", "UTCTime") -- ???
                , ("numeric string", "Text")
                , ("UTF-8 encoded string", "BS.ByteString")
                , ("numeric string or integer", "Text") -- ???
                , ("integer", "Integer")
                , ("list<unsigned int32>", "Vector Word32")
                , ("list<string>", "Vector Text")
                , ("list<numeric string>", "Vector Text")
                , ("list<numeric string or integer>", "Vector Text")
                , ("list<ExecOption>", "Vector ExecOption")
                , ("ISO 4217 Currency Code", "Money") -- ???
                , ("map<string, int32>", "Map.Map Text Int")
                , ("ConfigureStatus", "ConfigureStatusADT")
                , ("EffectiveStatus", "EffectiveStatusADT")
                , ("ConfiguredAdStatus", "ConfiguredAdStatus")
                , ("EffectiveAdStatus", "EffectiveAdStatus")
                , ("OptGoal", "OptGoal")
                , ("Targeting object", "TargetingSpecs")
                , ("Targeting", "TargetingSpecs")
                , ("BidType", "BidTypeADT")
                , ("DeleteStrategy", "DeleteStrategyADT")
                , ("Objective", "ObjectiveADT")
                , ("BuyingType", "BuyingTypeADT")
                , ("BillingEvent", "BillingEventADT")
                , ("CallActionType", "CallActionType")
                , ("URL", "Text")
                , ("post id", "Text")
                , ("Post ID", "Text")
                , ("post_id", "Text")
                , ("id", "Text")
                , ("AdAccount", "AdAccount") -- FIXME
                , ("AdCreativeId", "Text")
                , ("RunStatus", "RunStatusADT")
                , ("map<string, int32>", "Map.Map Text Int")
                , ("map<string, unsigned int32>", "Map.Map Text Word32")
                , ("dictionary", "A.Value") -- ???
                , ("Object", "A.Value")] -- ???
type ModeFieldInfoMap = Map.Map InteractionMode (Vector FieldInfo)
type EntityModeMap = Map.Map Entity ModeFieldInfoMap
newtype Env = Env EntityModeMap deriving Show

envsToMaps :: Vector Env -> Vector EntityModeMap
envsToMaps = coerce

buildEnv :: Vector (Vector CsvLine) -> Either Text Env
buildEnv csvs = do
    --let csvs' = join csvs :: Vector CsvLine
    let ignore = V.fromList $ ["rf_spec", "account_groups", "agency_client_declaration", "funding_source_details",
                              "owner_business", "business", "failed_delivery_checks", "permitted_roles", "access_type", "end_advertiser",
                              "currency"] -- Ad AccountA
                              ++
                              ["adlabels"] -- Campaign
                              ++
                              ["adset_schedule", "promoted_object", "campaign",
                              "product_ad_behavior", "rf_prediction_id", "pacing_type"]
                              ++ ["copy_from", "bytes", "zipbytes"] -- AdImage Create
                              ++ ["capabilities", "tos_accepted", "line_numbers", "bid_info"]
                              ++ ["image_crops", "object_story_spec", "object_type", "applink_treatment", "tracking_specs",
                                  "adset", "conversion_specs", "ad_review_feedback"]
                              ++ ["custom_event_type"]
                              ++ ["type", "dynamic_ad_voice", "annotations", "info_fields"]
                              ++ ["account"]
    --let csvs' = V.filter (\(CsvLine ent mode _) ->
    --               mode == Reading || ent == Entity "Ad Image" || ent == Entity "AdCreative") (join csvs :: Vector CsvLine)
    let csvs'' = V.filter (\(CsvLine _ _ (FieldInfo name _ _ _ _)) -> not $ V.elem name ignore) (join csvs :: Vector CsvLine)
    let envs = V.map buildEnvCsv csvs''
    let merged = merge envs
    let uni = unify merged
    Right uni
    --Right $ trace (show uni) uni

merge :: V.Vector Env -> Env -- Types Env and unified env
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
              val = case acc ^.at key of
                        Nothing  -> fromJust $ modeMap ^.at key -- mode is not in map
                        Just fis -> mergeFieldInfo fis $ fromJust $ modeMap ^.at key
          in acc & at key ?~ val
    | otherwise = error "updateModeMap"

-- mergeFieldInfo :: V n Fi -> V 1 Fi -> V (n+1) Fi
mergeFieldInfo :: Vector FieldInfo -> Vector FieldInfo -> Vector FieldInfo
mergeFieldInfo fis fiV = fiV V.++ fis

delFromEnv :: Env -> V.Vector FieldInfo -> Env
delFromEnv (Env env) del = Env $
    Map.map (\mode -> Map.map (\fis -> V.filter (\fi -> not $ V.elem fi del) fis) mode) env

unify :: Env -> Env
unify env =
    let fis = collectFieldInfos env
        dups = findDups fis
        unified = uni dups
    in if V.null unified
        then env
        else addTypesEnv env unified

collectFieldInfos :: Env -> V.Vector FieldInfo
collectFieldInfos (Env env) =
    P.foldl mergeFieldInfo V.empty $
        concat $ Map.elems $ Map.map (\ent -> Map.elems ent) env

collectFieldInfosMode :: ModeFieldInfoMap -> V.Vector FieldInfo
collectFieldInfosMode mode =
    P.foldl mergeFieldInfo V.empty $ Map.elems mode

addTypesEnv :: Env -> Vector FieldInfo -> Env
addTypesEnv (Env env) types = Env $
    Map.insert (Entity "Types") (Map.insert Types types Map.empty) env

-- returns the FieldInfos to be updated to use the fully-qualified, globally defined types
-- instead of defining the same types all over locally.
uni :: [V.Vector FieldInfo] -> V.Vector FieldInfo
uni dups = V.fromList $ go dups []
    where
        go [] acc = acc
        go (ds:dss) acc =
            let cur = V.head ds
            in if V.all (\fi -> type_ fi == type_ cur) ds
                then go dss $ cur:acc
                else go dss acc

-- returns all duplicate FieldInfos
findDups :: V.Vector FieldInfo -> [V.Vector FieldInfo]
findDups fis = go fis []
    where
        go fis acc
            | V.null fis = acc
            | otherwise =
                let fi = V.head fis
                    tail = V.tail fis
                    dupInds = V.findIndices (==fi) tail
                in if V.null dupInds
                    then go tail acc
                    else let dups = V.cons fi $ V.map (\idx -> unsafeIndex tail idx) dupInds
                             tail' = V.ifilter (\idx _ -> not $ V.elem idx dupInds) tail
                         in go tail' $ dups:acc

removeNameTypeDups :: V.Vector FieldInfo -> V.Vector FieldInfo
removeNameTypeDups fi = removeDups fi (\cur e -> e == cur && type_ e == type_ cur)

removeNameDups :: V.Vector FieldInfo -> V.Vector FieldInfo
removeNameDups fi = removeDups fi (\cur e -> e == cur)

removeDups :: V.Vector FieldInfo -> (FieldInfo -> FieldInfo -> Bool) -> V.Vector FieldInfo
removeDups fis pred = V.reverse $ V.fromList $ go fis []
    where
        go fis acc
            | V.null fis = acc
            | otherwise =
                let fi = V.head fis
                    tail = V.tail fis
                    dupInds = V.findIndices (pred fi) tail
                in if V.null dupInds
                    then go tail $ fi:acc
                    else let dups = V.cons fi $ V.map (\idx -> unsafeIndex tail idx) dupInds
                             tail' = V.ifilter (\idx _ -> not $ V.elem idx dupInds) tail
                         in go tail' $ fi:acc
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
