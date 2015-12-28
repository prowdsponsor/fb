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

import Facebook.Gen.Environment
import Facebook.Gen.Types
import Facebook.Gen.CodeGenStrTypes

import Debug.Trace

typesImport = "import Facebook.Object.Marketing.Types"

imports =
    V.fromList ["import Facebook.Records",
                "import Facebook.Types hiding (Id)",
                "import Facebook.Pager",
                "import Facebook.Monad",
                "import Facebook.Graph",
                "import qualified Data.Aeson as A",
                "import Data.Time.Clock",
                "import Data.Text (Text)",
                "import Data.Word (Word32)",
                "import GHC.Generics (Generic)",
                "import qualified Data.Map.Strict as Map",
                "import Data.Vector (Vector)",
                "import qualified Control.Monad.Trans.Resource as R",
                "import Control.Monad.Trans.Control (MonadBaseControl)"]
langExts = V.fromList ["DeriveDataTypeable", "DeriveGeneric", "FlexibleContexts", "OverloadedStrings",
                       "ConstraintKinds"]

-- What do add after /id to the URL
entityUrlPostfixMap =
    Map.fromList [(Entity "AdCampaign", "/campaigns"),
                  (Entity "Insights", "/insights"),
                  (Entity "AdSet", "/adsets")]

-- Does the generated function return a Pager?
entityModePagerSet =
    Set.fromList [(Entity "AdCampaign", Reading),
                  (Entity "Insights", Reading),
                  (Entity "AdSet", Reading)]

-- Doees the API call need a token?
isTokenNecessarySet =
    Set.fromList [(Entity "AdCampaign", Reading),
                  (Entity "AdSet", Reading)]

modPrefix = "Facebook.Object.Marketing."
modNameToPath = replace "." "/"

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
                    else ""
        fis = collectFieldInfosMode map
        filter x = if nameEnt == "Types"
                    then types
                    else V.filter (\fi -> not $ V.elem fi types) x
        filtered = filter fis
        dataDecl = V.foldl' append "" $ V.map dataAndFieldInstance filtered
        getter = concat $ V.map getterFct filtered
        getterFct fi = getterField fi
        concat = V.foldl' append ""
        -- gen all data fields + instances for records here TODO
    in (path, top <> dataDecl <> getter <> Prelude.foldl append "" (
            Map.elems $ Map.mapWithKey (\mode fis -> genMode ent mode fis) map))

genMode :: Entity -> InteractionMode -> V.Vector FieldInfo -> Text
genMode _ Types _ = T.empty -- Types.hs doesn't include any functions (except record getters)
genMode ent@(Entity nameEnt) mode unfiltered = -- source code for one entity(/mode ?)
    let -- source = V.foldl' append "" $ V.map dataAndFieldInstance fis -- TODO filter out globally defined ones
        doc = "\n-- Entity:" <> nameEnt <> ", mode:" <> T.pack (show mode)
        m = genFcts mode ent unfiltered
        isInstances = genClassWitnesses ent mode unfiltered
    in doc <> isInstances <> m <> if nameEnt == "AdAccount"
                                      then adAccIdDetails <> genGetId
                                      else ""

genClassWitnesses :: Entity -> InteractionMode -> Vector FieldInfo -> Text
genClassWitnesses _ Types _ = T.empty
genClassWitnesses ent mode fis =
    let isOfClass = isFieldClass ent mode
        isOfInstances = concat $ V.map (\fi -> instanceFct fi) fis
        nilIsOfInstance = isFieldClassInstanceText (entityToIsField ent mode) "Nil" -- ugly hack
        instanceFct field = isFieldClassInstance ent mode field
        --constr = genConstraint mode fis entity
        concat = V.foldl' append ""
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
    in "\ntype " <> genClassName ent mode <> " fl r = (" <> reqsHas
       <> "A.FromJSON r, " <> entityToIsField ent mode <> " r, FieldListToRec fl r)"

-- "acc_id" and Text turn into:
--
-- data AccId = AccId
-- instance Field AccId where
--  type FieldValue AccId = Text
--  fieldName _ = "acc_id"
--  fieldLabel  = AccId
dataAndFieldInstance :: FieldInfo -> Text
dataAndFieldInstance fi =
    let adtName = fieldToAdt fi
        newtypeName = adtName <> "_"
        fieldType = type_ fi
        fieldName = name fi
    in "\ndata "  <> adtName <> " = " <> adtName <> "\n"
        <> "newtype " <> newtypeName <> " = " <> newtypeName
        <> " " <> fieldTypeParan fieldType <> " deriving " <> derivings fieldType <> "\n"
        <> newtypeJSON newtypeName
        <> "instance Field " <> adtName <> " where\n\t"
        <> "type FieldValue " <> adtName <> " = " <> newtypeName <> "\n\t"
        <> "fieldName _ = \"" <> fieldName <> "\"\n\t"
        <> "fieldLabel = " <> adtName <> "\n"

fieldTypeParan :: Text -> Text
fieldTypeParan ft =
    if Prelude.length (split (==' ') ft) > 1
        then "(" <> ft <> ")"
        else ft

derivings :: Text -> Text
derivings "UTCTime" = "Generic"
derivings _ = "(Show, Generic)"

newtypeJSON :: Text -> Text
newtypeJSON nt =
    "instance A.FromJSON " <> nt <> "\n"
    <> "instance A.ToJSON " <> nt <> "\n"

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
    in "\n" <> fieldName <> " r = r `get` " <> adtName

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
    let pager' = if Set.member (ent, mode) entityModePagerSet
                    then "(Pager r)"
                    else "r"
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

genFct :: Entity -> InteractionMode -> Text -> Text
genFct ent mode defFields =
    let fctName = genFctName ent mode
        url  = Map.findWithDefault "" ent entityUrlPostfixMap
        maybeToken = if Set.member (ent, mode) isTokenNecessarySet
                        then "$ Just "
                        else ""
    in fctName <> " (Id_ id) fl mtoken = getObject (\"/v2.5/\" <> id <> \"" <> url
       <> "\") [(\"fields\", textListToBS $ fieldNameList $ " <> defFields <> "fl)] " <>  maybeToken <> "mtoken\n\n"

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
