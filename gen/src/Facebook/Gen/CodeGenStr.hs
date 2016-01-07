module Facebook.Gen.CodeGenStr
    --(genFiles)
where

import Data.Monoid ((<>))
import Data.Text
import qualified Data.Text as T
import Data.Vector hiding (singleton)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Facebook.Gen.Environment
import Facebook.Gen.Types
import Facebook.Gen.CodeGenStrTypes

import Debug.Trace

typesImport = "import Facebook.Object.Marketing.Types"

imports =
    V.fromList ["import Facebook.Records hiding (get)",
                "import qualified Facebook.Records as Rec",
                "import Facebook.Types hiding (Id)",
                "import Facebook.Pager",
                "import Facebook.Monad",
                "import Facebook.Graph",
                "import qualified Data.Aeson as A",
                "import Data.Time.Clock",
                "import Data.Time.Format",
                "import Data.Aeson hiding (Value)",
                "import Control.Applicative",
                "import Data.Text (Text)",
                "import Data.Text.Read (decimal)",
                "import Data.Scientific (toBoundedInteger)",
                "import qualified Data.Text.Encoding as TE",
                "import GHC.Generics (Generic)",
                "import qualified Data.Map.Strict as Map",
                "import Data.Vector (Vector)",
                "import qualified Data.Vector as V",
                "import qualified Data.ByteString as BS",
                "import qualified Data.ByteString.Char8 as B8",
                "import qualified Data.ByteString.Builder as BSB",
                "import qualified Data.ByteString.Lazy as BSL",
                "import qualified Control.Monad.Trans.Resource as R",
                "import Control.Monad.Trans.Control (MonadBaseControl)"]
langExts = V.fromList ["DeriveDataTypeable", "DeriveGeneric", "FlexibleContexts", "OverloadedStrings",
                       "ConstraintKinds"]

-- What do add after /id to the URL
entityUrlPostfixMap =
    Map.fromList [(Entity "AdCampaign", "/campaigns"),
                  (Entity "Insights", "/insights"),
                  (Entity "AdImage", "/adimages"),
                  (Entity "Ad", "/ads"),
                  (Entity "AdCreative", "/adcreatives"),
                  (Entity "AdSet", "/adsets")]

-- Does the generated function return a Pager?
entityModePagerSet =
    Set.fromList [(Entity "AdCampaign", Reading),
                  (Entity "Insights", Reading),
                  (Entity "AdImage", Reading),
                  (Entity "Ad", Reading),
                  (Entity "AdSet", Reading)]

-- Does the generated function return a Pager?
entityModeRetType =
    Map.fromList [((Entity "AdImage", Creating), "SetImgs"),
                  ((Entity "AdImage", Deleting), "Success"),
                  ((Entity "AdCampaign", Deleting), "Success"),
                  ((Entity "AdCampaign", Creating), "CreateCampaignId")]


-- Does the generated function return a Pager?
idTypeMap =
    Map.fromList [((Entity "AdCampaign", Deleting), "CreateCampaignId")]

-- Does the generated function return a Pager?
entityModeRetDefs :: Map.Map (Entity, InteractionMode) Text
entityModeRetDefs =
    Map.fromList [((Entity "AdImage", Creating), imgCreate),
                  ((Entity "AdCampaign", Creating), campaignCreate)]

imgCreate = "data SetImgs = SetImgs { -- as seen when using curl\n\
                \\timages  :: Map.Map Text SetImg\n\
                \\t} deriving (Show, Generic)\n\
            \instance FromJSON SetImgs\n\
            \data SetImg = SetImg {\n\
                \\thash, url_ :: Text\n\
            \\t} deriving Show\n\
            \instance FromJSON SetImg where\n\
                \\tparseJSON (Object v) =\n\
                    \\t\tSetImg <$> v .: \"hash\"\n\
                           \\t\t\t\t<*> v .: \"url\"\n"

imgDelete =  "data Success = Success {\n\
              \\tsuccess :: Bool\n\
              \\t} deriving (Show, Generic)\n\
              \instance FromJSON Success"

campaignCreate =
    "data CreateCampaignId = CreateCampaignId {\n\
     \\tcampaignId :: Text\n\
     \\t} deriving (Show, Generic)\n\
     \instance FromJSON CreateCampaignId where\n\
     \\t\tparseJSON (Object v) =\n\
     \\t\t   CreateCampaignId <$> v .: \"id\"\n"

-- Doees the API call need a token?
isTokenNecessarySet =
    Set.fromList [(Entity "AdCampaign", Reading),
                  (Entity "AdCampaign", Creating),
                  (Entity "AdCampaign", Updating),
                  (Entity "AdCampaign", Deleting),
                  (Entity "AdImage", Reading),
                  (Entity "AdImage", Creating),
                  (Entity "AdAccount", Creating),
                  (Entity "AdAccount", Updating),
                  (Entity "AdAccount", Deleting),
                  (Entity "AdImage", Deleting),
                  (Entity "Ad", Deleting),
                  (Entity "Ad", Updating),
                  (Entity "Ad", Creating),
                  (Entity "AdCreative", Deleting),
                  (Entity "AdCreative", Updating),
                  (Entity "AdCreative", Creating),
                  (Entity "AdSet", Deleting),
                  (Entity "AdSet", Updating),
                  (Entity "AdSet", Creating),
                  (Entity "AdSet", Reading)]

modPrefix = "Facebook.Object.Marketing."
modNameToPath = replace "." "/"

-- needed for POST
toBsInstances :: Text
toBsInstances = -- FIXME, look at old SimpleType class for instances
  "\ninstance ToBS Text where\n\
  \\ttoBS = TE.encodeUtf8\n\
  \instance ToBS Char where\n\
  \\ttoBS = B8.singleton\n\
  \instance ToBS Integer\n\
  \instance ToBS Int\n\
  \instance ToBS Bool\n\
  \instance ToBS A.Value\n\
  \instance ToBS Float\n\
  \instance ToBS a => ToBS (Vector a) where\n\
  \\ttoBS xs = V.foldl' BS.append BS.empty $ V.map toBS xs\n\
  \instance ToBS UTCTime where\n\
  \\ttoBS t = B8.pack $ formatTime defaultTimeLocale rfc822DateFormat t\n"

genFiles :: Env -> Vector (FilePath, Text)
genFiles (Env env) =
    let (typesMap, rest) = Map.partitionWithKey (\k _ -> k == Entity "Types") env
    in if Map.null typesMap -- there won't be a Types module
        then genCode (Env env) V.empty
        else let typesCode = genTypes typesMap
                 typesFieldInfos = (typesMap Map.! (Entity "Types")) Map.! Types
             in genCode (Env env) typesFieldInfos

genCode :: Env -> Vector FieldInfo -> Vector (FilePath, Text)
genCode (Env env) types =
    V.fromList $ Map.elems $ Map.mapWithKey (\k a -> genEntity k a types) env

genTypes :: EntityModeMap -> (FilePath, Text)
genTypes entMap =
    let typesCode = genEntity typesEnt Map.empty V.empty
        typesEnt = Prelude.head $ Map.keys entMap
    in typesCode

genEntity :: Entity -> ModeFieldInfoMap -> V.Vector FieldInfo -> (FilePath, Text)
genEntity ent@(Entity nameEnt) map types =
    let modName = modPrefix <> nameEnt
        path = T.unpack $ modNameToPath modName <> ".hs"
        head = header modName
        top = genLangExts <> head <> genImports ent <>
                if nameEnt /= "Types"
                    then typesImport <> "\n" -- FIXME
                    else toBsInstances
        fis = collectFieldInfosMode map
        filter x = if nameEnt == "Types"
                    then types
                    else V.filter (\fi -> not $ V.elem fi types) x
        filtered = filter fis
        --dataDecl = V.foldl' append "" $ V.map dataAndFieldInstance $ removeDups filtered
        dataDecl = dataAndFieldInstances $ removeNameTypeDups filtered
        getter = myConcat $ V.map getterFct $ removeNameDups filtered
        getterFct fi = getterField fi
        bsInstances = genToBsInstances $ removeNameDups filtered
            -- genToBsInstances $ collectFieldInfosMode $ Map.delete Reading map -- collect all fields that need to be transmitted to FB
        -- gen all data fields + instances for records here TODO
    in (path, top <> dataDecl <> bsInstances <> getter <> Prelude.foldl append "" (
            Map.elems $ Map.mapWithKey (\mode fis -> genMode ent mode fis) map))

genMode :: Entity -> InteractionMode -> V.Vector FieldInfo -> Text
genMode _ Types _ = T.empty -- Types.hs doesn't include any functions (except record getters)
genMode ent@(Entity nameEnt) mode unfiltered = -- source code for one entity(/mode ?)
    let -- source = V.foldl' append "" $ V.map dataAndFieldInstance fis -- TODO filter out globally defined ones
        doc = "\n-- Entity:" <> nameEnt <> ", mode:" <> T.pack (show mode)
        retDef = getRetDef ent mode
        toBS = ""
                --if mode /= Reading
                --then genToBsInstances unfiltered
                --else ""
        m = genFcts mode ent unfiltered
        isInstances = genClassWitnesses ent mode unfiltered
    in doc <> toBS <> isInstances <> retDef <> m <>
        if nameEnt == "AdAccount" && mode == Reading
            then adAccIdDetails <> genGetId
            else ""

myConcat :: V.Vector Text -> Text
myConcat = V.foldl' append ""

genToBsInstances :: V.Vector FieldInfo -> Text
genToBsInstances fis = myConcat $ V.map go fis
    where
        go fi =
         let nt = fieldToAdt fi <> "_" -- FIXME
         in "\ninstance ToBS " <> nt <> " where\n"
            <> "\ttoBS (" <> nt <> " a) = toBS a\n"

getRetDef :: Entity -> InteractionMode -> Text
getRetDef ent mode =
    case Map.lookup (ent, mode) entityModeRetDefs of
        Nothing -> ""
        Just code -> code

genClassWitnesses :: Entity -> InteractionMode -> Vector FieldInfo -> Text
genClassWitnesses _ Types _ = T.empty
genClassWitnesses ent mode fis =
    let isOfClass = isFieldClass ent mode
        isOfInstances = myConcat $ V.map (\fi -> instanceFct fi) fis
        nilIsOfInstance = isFieldClassInstanceText (entityToIsField ent mode) "Nil" -- ugly hack
        instanceFct field = isFieldClassInstance ent mode field
        --constr = genConstraint mode fis entity
    in isOfClass <> nilIsOfInstance <> isOfInstances

genReqFields :: Vector FieldInfo -> Vector Text
genReqFields fis = V.map fieldToAdt $ V.filter isRequired fis

modeStr :: InteractionMode -> Text
modeStr Reading = "Get"
modeStr Deleting = "Del"
modeStr Updating = "Upd"
modeStr Creating = "Set"
modeStr Types = error "FIXME"

genFcts :: InteractionMode -> Entity -> V.Vector FieldInfo -> Text
genFcts mode@Reading ent fis =
  let constr = genConstraint mode V.empty ent
      retConstr = genRetConstraint mode ent fis <> " -- Default fields"
      fctType = getFctType ent mode
      fct = genFct ent mode $ genDefFields fis
  in constr <> "\n" <> retConstr <> "\n" <> fctType <> "\n" <> fct
genFcts mode ent fis =
    let constr = genConstraint mode fis ent
        fctType = getFctType ent mode
        fct = genFct ent mode "" -- quick and dirty
    in constr <> "\n" <> fctType <> "\n" <> fct

genDefFields :: V.Vector FieldInfo -> Text
genDefFields fis =
  let ds = genReqFields fis
      defs = V.foldr' append "" $ V.map (\req -> req <> " ::: ") ds
  in defs

genRetConstraintName :: InteractionMode -> Entity -> Text
genRetConstraintName Reading ent =
  genClassName ent Reading <> "Ret"

genRetConstraint :: InteractionMode -> Entity -> V.Vector FieldInfo -> Text
genRetConstraint Reading ent fis =
  let ds = genReqFields fis
      defs = V.foldr' append "r" $ V.map (\req -> req <> " :*: ") ds
      synName = genRetConstraintName Reading ent
  in "type " <> synName <> " r = " <> defs

header modName = "module " <> modName <> " where\n\n"

concatNewline xs = V.foldl' append "" $ V.map (\x -> x <> "\n") xs

genImports (Entity "Types")  = concatNewline imports <> oldTypesImport <> oldTypes <> newTypes
genImports _ = concatNewline imports
genLangExts = concatNewline $ V.map (\x -> "{-# LANGUAGE " <> x <> " #-}") langExts

genConstraint :: InteractionMode -> Vector FieldInfo -> Entity -> Text
genConstraint Types _ _ = ""
genConstraint mode fis ent = genConstraintLabel mode fis ent

genConstraintLabel :: InteractionMode -> Vector FieldInfo -> Entity -> Text
genConstraintLabel mode fis ent@(Entity entity) =
    let reqs  = genReqFields fis
        reqsHas = V.foldl' append "" $ V.map (\req -> "Has " <> req <> " r, ") reqs
        arg = if mode == Reading
                then " fl r"
                else " r"
        constr = if mode == Reading
                  then "FieldListToRec fl r)"
                  else "ToForm r)"
    in "\ntype " <> genClassName ent mode <> arg <> " = (" <> reqsHas
       <> "A.FromJSON r, " <> entityToIsField ent mode <> " r, " <> constr

-- "acc_id" and Text turn into:
--
-- data AccId = AccId
-- instance Field AccId where
--  type FieldValue AccId = Text
--  fieldName _ = "acc_id"
--  fieldLabel  = AccId
dataAndFieldInstances :: V.Vector FieldInfo -> Text
dataAndFieldInstances fis =
    let dups = findDups fis
    in if List.null dups -- Do all fields with the same name have the same type?
        then let dataDecls = myConcat $ V.map dataAndFieldInstance fis
                 new = myConcat $ V.map newtypeInstances fis
             in dataDecls <> new
        else -- this usually happens because Reading mode returns strings while Creating expects unsigned int32
            let dups' = V.concat dups
                unique = V.filter (\fi -> not $ List.elem fi dups') fis -- check
                a@(maxs, mins) = (List.map List.maximum dups, --FIXME... let's hope we only have to convert to one type...
                                List.map List.minimum dups)
                dataDecls = myConcat $ V.map dataAndFieldInstance unique
                new = myConcat $ V.map newtypeInstances unique
                maxNew = T.concat $ List.map dataAndFieldInstance maxs -- these are the real types of our data types
                jsonMax = genJsonNewtype $ List.zip maxs mins
            in dataDecls <> new <> maxNew <> jsonMax

genJsonNewtype :: [(FieldInfo, FieldInfo)] -> Text
genJsonNewtype fis =
    let types = List.map (\(f1, f2) -> (newtypeName f1, type_ f1, type_ f2)) fis
    in T.concat $ List.map typesToJsonInstances types

typesToJsonInstances :: (Text, Text, Text) -> Text
typesToJsonInstances (nt, "Int", "Text") =
    let create = "pure $ " <> nt <> " num"
    in "instance A.FromJSON " <> nt <> " where\n\
        \\tparseJSON (Number x) =\n\
        \\t case toBoundedInteger x of\n\
        \\t   Just num -> " <> create <> "\n" <>
        "\t   Nothing -> error \"Well... awesome\"\n\
        \\tparseJSON (String str) =\n\
        \\t case decimal str of\n\
        \\t   Left err -> error err\n\
        \\t   Right (num, _) -> " <> create <> "\n" <> -- FIXME
        "instance A.ToJSON " <> nt <> "\n"
typesToJsonInstances x = error $ show x

dataAndFieldInstance :: FieldInfo -> Text
dataAndFieldInstance fi =
    let adtName = fieldToAdt fi
        nt = newtypeName fi
        fieldType = type_ fi
        fieldName = name fi
    in "\ndata "  <> adtName <> " = " <> adtName <> "\n"
        <> "newtype " <> nt <> " = " <> nt
        <> " " <> fieldTypeParan fieldType <> " deriving " <> derivings fieldType <> "\n"
        -- <> newtypeInstances newtypeName
        <> "instance Field " <> adtName <> " where\n\t"
        <> "type FieldValue " <> adtName <> " = " <> nt <> "\n\t"
        <> "fieldName _ = \"" <> fieldName <> "\"\n\t"
        <> "fieldLabel = " <> adtName <> "\n"

newtypeName :: FieldInfo -> Text
newtypeName fi =
    let adtName = fieldToAdt fi
    in adtName <> "_"

fieldTypeParan :: Text -> Text
fieldTypeParan ft =
    if Prelude.length (split (==' ') ft) > 1
        then "(" <> ft <> ")"
        else ft

derivings :: Text -> Text
derivings "UTCTime" = "Generic"
derivings _ = "(Show, Generic)"

newtypeInstances :: FieldInfo -> Text
newtypeInstances fi =
    let nt = newtypeName fi
    in "instance A.FromJSON " <> nt <> "\n"
        <> "instance A.ToJSON " <> nt <> "\n"
    -- <> "instance ToBS " <> nt <> " where\n\t"
    -- <> "toBS (" <> nt <> " a) = toBS a\n"

entityToIsField :: Entity -> InteractionMode -> Text
entityToIsField (Entity entity) mode = "Is" <> entity <> modeStr mode <> "Field"

isFieldClass :: Entity -> InteractionMode -> Text
isFieldClass entity mode =
    let className = entityToIsField entity mode
    in "\nclass " <> className <> " r\n"
       <> "instance (" <> className <> " h, "
       <> className <> " t) => " <> className
       <> " (h :*: t)\n"

isFieldClassInstanceText :: Text -> Text -> Text
isFieldClassInstanceText className instName =
    "instance " <> className <> " " <> instName <> "\n"

isFieldClassInstance :: Entity -> InteractionMode -> FieldInfo -> Text
isFieldClassInstance ent mode fi =
    let className = entityToIsField ent mode
        instName = fieldToAdt fi
    in isFieldClassInstanceText className instName

genClassName :: Entity -> InteractionMode -> Text
genClassName (Entity e) mode =
    let mStr = modeStr mode
    in e <> mStr

getterField :: FieldInfo -> Text
getterField fi =
    let fieldName = name fi
        adtName = fieldToAdt fi
    in "\n" <> fieldName <> " r = r `Rec.get` " <> adtName

adAccIdDetails :: Text
adAccIdDetails = "type AdAccountIdDetails = Id :*: AccountId :*: Nil\n"

genGetId :: Text
genGetId =
    "\ngetAdAccountId :: (R.MonadResource m, MonadBaseControl IO m) =>\n\t\
              \ Maybe UserAccessToken -- ^ User access token.\n\t\
            \-> FacebookT anyAuth m (Pager AdAccountIdDetails)\n\
    \getAdAccountId token = getObject \"/v2.5/me/adaccounts\" [] token\n"

genFctName :: Entity -> InteractionMode -> Text
genFctName _ Types = ""
genFctName (Entity ent) Reading = "get" <> ent
genFctName (Entity ent) Deleting = "del" <> ent
genFctName (Entity ent) Updating = "upd" <> ent
genFctName (Entity ent) Creating = "set" <> ent

getFctType :: Entity -> InteractionMode -> Text
getFctType ent mode@Reading =
  let retType = genRetConstraintName mode ent
      pager' = if Set.member (ent, mode) entityModePagerSet
                  then "(Pager (" <> retType <> " r))"
                  else "(" <> retType <> " r)"
      maybeToken = if Set.member (ent, mode) isTokenNecessarySet
                      then ""
                      else "Maybe"
      fctName = genFctName ent mode
      className = genClassName ent mode
  in
  fctName <> " :: (R.MonadResource m, MonadBaseControl IO m, " <> className <> " fl r) =>\n\t\
               \Id_    -- ^ Ad Account Id\n\t\
            \-> fl     -- ^ Arguments to be passed to Facebook.\n\t\
            \-> " <> maybeToken <> " UserAccessToken -- ^ Optional user access token.\n\t\
            \-> FacebookT anyAuth m " <> pager'
getFctType ent mode =
    let retType = case Map.lookup (ent, mode) entityModeRetType of
                    Nothing -> "r"
                    Just ret -> ret
        pager' = if Set.member (ent, mode) entityModePagerSet
                    then "(Pager " <> retType <> ")"
                    else retType
        maybeToken = if Set.member (ent, mode) isTokenNecessarySet
                        then ""
                        else "Maybe"
        fctName = genFctName ent mode
        className = genClassName ent mode
        auth = if mode == Reading
                then "anyAuth"
                else "Auth"
        argName = if mode == Reading
                    then "fl"
                    else "r"
        param = if mode == Reading
                    then " fl r"
                    else " r"
        idConstr = case Map.lookup (ent, mode) idTypeMap of
                    Just x -> x
                    Nothing -> "Id_"
    in
    fctName <> " :: (R.MonadResource m, MonadBaseControl IO m, " <> className <> param <> ") =>\n\t"
            <> idConstr <> "    -- ^ Ad Account Id\n\t\
            \-> " <> argName <> "     -- ^ Arguments to be passed to Facebook.\n\t\
            \-> " <> maybeToken <> " UserAccessToken -- ^ Optional user access token.\n\t\
            \-> FacebookT " <> auth <> " m " <> pager'

genFct :: Entity -> InteractionMode -> Text -> Text
genFct ent mode defFields =
    let fctName = genFctName ent mode
        url  = Map.findWithDefault "" ent entityUrlPostfixMap
        maybeToken = if Set.member (ent, mode) isTokenNecessarySet && mode == Reading
                        then "$ Just "
                        else ""
        httpMethod = modeToMethod mode
        args = modeToArgs mode
        argName = if mode == Reading
                    then "fl"
                    else "r"
        idConstr = case Map.lookup (ent, mode) idTypeMap of
                    Just x -> x
                    Nothing -> "Id_"
    in fctName <> " (" <> idConstr <> " id) " <> argName <> " mtoken = " <> httpMethod <> " (\"/v2.5/\" <> id <> \"" <> url
       <> "\") " <> args defFields <> maybeToken <> "mtoken\n\n"

modeToArgs Types _ = ""
modeToArgs Reading defFields = "[(\"fields\", textListToBS $ fieldNameList $ " <> defFields <> "fl)] "
modeToArgs Creating _ = "(toForm r) "
modeToArgs Updating _ = "(toForm r) "
modeToArgs Deleting _ = "(toForm r) "

modeToMethod :: InteractionMode -> Text
modeToMethod Reading = "getObject"
modeToMethod Creating = "postForm"
modeToMethod Updating = "postForm"
modeToMethod Deleting = "deleteForm"

fieldToAdt :: FieldInfo -> Text
fieldToAdt (FieldInfo str _ _ _ _)
    | T.null $ dropUnderscore str = str
    | otherwise =
        let str' = dropUnderscore str
            first = charToUpper $ T.head str'
            camel = toCamelCase "" $ T.tail str'
        in first <> camel
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
                    then toCamelCase (acc <> a) b'
                    else toCamelCase (acc <> a <> first) $ T.tail b'
