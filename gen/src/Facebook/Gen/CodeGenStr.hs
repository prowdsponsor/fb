module Facebook.Gen.CodeGenStr
    --(genFiles)
where

import Data.Monoid ((<>))
import Data.Text
import qualified Data.Text as T
import Data.Vector hiding (singleton)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

import Facebook.Gen.Environment
import Facebook.Gen.Types

imports =
    V.fromList ["import Facebook.Records",
                "import Facebook.Types hiding (Id)",
                --"import Facbook.Object.Marketing.Types", -- this file is generated
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

modPrefix = "Facebook.Object.Marketing."
modNameToPath = replace "." "/"

--genFiles :: Env -> Vector (FilePath, Text)
genFiles (Env env) =
    let code = Map.elems $ Map.mapWithKey genEntity env
    in  code

--genEntity :: MapInEnv ->
genEntity (Entity name) map =
    let modName = modPrefix <> name
        path = modNameToPath modName
        head = header modName
        top = genLangExts <> head <> genImports
    in (path, Prelude.head $ Map.elems $ Map.mapWithKey (\mode elem -> top <> genMode name mode elem) map)

genMode :: Text -> InteractionMode -> V.Vector FieldInfo -> Text
genMode entity mode fis = -- source code for one entity(/mode ?)
    let source = V.foldl' append "" $ V.map dataAndFieldInstance fis
        isOfClass = isFieldClass entity
        isOfInstances = concat $ V.map (\fi -> instanceFct $ fieldToAdt $ name fi) fis
        nilIsOfInstance = isFieldClassInstance (entityToIsField entity) "Nil"
        instanceFct field = isFieldClassInstance (entityToIsField entity) field
        getter = concat $ V.map getterFct fis
        getterFct fi = getterField (name fi) (fieldToAdt $ name fi)
        constr = genConstraint mode (genReqFields fis) entity
        concat = V.foldl' append ""
    in source <> isOfClass <> nilIsOfInstance <> isOfInstances <> getter
       <> constr <> adAccIdDetails <> genGetId <> genGetFct entity

genReqFields :: Vector FieldInfo -> Vector Text
genReqFields fis =
    V.map (fieldToAdt . name) $ V.filter isRequired fis

header modName = "module " <> modName <> " where\n\n"

concatNewline xs = V.foldl' append "" $ V.map (\x -> x <> "\n") xs

genImports  = concatNewline imports
genLangExts = concatNewline $ V.map (\x -> "{-# LANGUAGE " <> x <> " #-}") langExts

genConstraint :: InteractionMode -> Vector Text -> Text -> Text
genConstraint (InteractionMode "Reading") r e = genConstraintGet r e
genConstraint _ _ _ = error "Only InteractionMode Reading supported"

genConstraintGet :: Vector Text -> Text -> Text
genConstraintGet reqs entity =
    let reqsHas = V.foldl' append "" $ V.map (\req -> "Has " <> req <> " r, ") reqs
    in "\ntype Get" <> entity <> " fl r = (" <> reqsHas
       <> "A.FromJSON r, " <> entityToIsField entity <> " r, FieldListToRec fl r)\n\n"

-- "acc_id" and Text turn into:
--
-- data AccId = AccId
-- instance Field AccId where
--  type FieldValue AccId = Text
--  fieldName _ = "acc_id"
--  fieldLabel  = AccId
dataAndFieldInstance :: FieldInfo -> Text
dataAndFieldInstance (FieldInfo fieldName fieldType _ _ _) =
    let adtName = fieldToAdt fieldName
        newtypeName = adtName <> "_"
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

entityToIsField entity = "Is" <> entity <> "Field"

isFieldClass entity =
    let className = entityToIsField entity
    in "\nclass " <> className <> " r\n"
       <> "instance (" <> className <> " h, "
       <> className <> " t) => " <> className
       <> " (h :*: t)\n"

isFieldClassInstance className instName =
    "instance " <> className <> " " <> instName <> "\n"

getterField fieldName adtName =
    "\n" <> fieldName <> " r = r `get` " <> adtName

adAccIdDetails = "type AdAccountIdDetails = Id :*: AccountId :*: Nil\n"

genGetId :: Text
genGetId =
    "\ngetAdAccountId :: (R.MonadResource m, MonadBaseControl IO m) =>\n\t\
              \UserAccessToken -- ^ User access token.\n\t\
            \-> FacebookT anyAuth m (Pager AdAccountIdDetails)\n\
    \getAdAccountId token = getObject \"/v2.5/me/adaccounts\" [] (Just token)\n"

genGetFct ent =
    "\nget" <> ent <> " :: (R.MonadResource m, MonadBaseControl IO m, Get" <> ent <> " fl r) =>\n\t\
               \Id_    -- ^ Ad Account Id\n\t\
            \-> fl     -- ^ Arguments to be passed to Facebook.\n\t\
            \-> Maybe UserAccessToken -- ^ Optional user access token.\n\t\
            \-> FacebookT anyAuth m r\n\
    \get" <> ent <> " (Id_ id) fl mtoken = getObject (\"/v2.5/\" <> id) [(\"fields\", textListToBS $ fieldNameList fl)] mtoken\n"

fieldToAdt :: Text -> Text
fieldToAdt str
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
