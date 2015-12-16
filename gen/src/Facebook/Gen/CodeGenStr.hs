module Facebook.Gen.CodeGenStr
    --(genFiles)
where

import Control.Monad
import Data.Text
import qualified Data.Text as T
import Data.Vector hiding (singleton)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

import Facebook.Gen.Environment
import Facebook.Gen.Types

imports = V.fromList ["import Facebook.Records", "import qualified Data.Aeson as A", "import Data.Thyme.Clock"]
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
    let modName = modPrefix `append` name
        path = modNameToPath modName
        head = header modName
        top = genLangExts `append` head `append` genImports
    in (path, Prelude.head $ Map.elems $ Map.mapWithKey (\mode elem -> top `append` genMode name mode elem) map)

genMode :: Text -> InteractionMode -> V.Vector FieldInfo -> Text
genMode entity mode fis = -- source code for one entity(/mode ?)
    let source = V.foldl' append "" $ V.map dataAndFieldInstance fis
        constr = genConstraint mode (genReqFields fis) entity
    in source `append` constr

genReqFields :: Vector FieldInfo -> Vector Text
genReqFields fis =
    V.map (fieldNameToAdtName . name) $ V.filter isRequired fis

header modName = "module " `append` modName `append` " where\n\n"

concatNewline xs = V.foldl' append "" $ V.map (\x -> x `append` "\n") xs

genImports  = concatNewline imports
genLangExts = concatNewline $ V.map (\x -> "{-# LANGUAGE " `append` x `append` " #-}") langExts

--getAdAccountId :: (R.MonadResource m, MonadBaseControl IO m) =>
--          UserAccessToken -- ^ User access token.
--        -> FacebookT anyAuth m (Pager (AdId :*: AdAccId :*: Nil))
--getAdAccountId token = getObject "/v2.5/me/adaccounts" [] (Just token)

genConstraint :: InteractionMode -> Vector Text -> Text -> Text
genConstraint (InteractionMode "Reading") r e = genConstraintGet r e
genConstraint _ _ _ = error "Only InteractionMode Reading supported"

genConstraintGet :: Vector Text -> Text -> Text
genConstraintGet reqs entity =
    let reqsHas = V.foldl' append "" $ V.map (\req -> "Has " `append` req `append` " r, ") reqs
    in "type Get" `append` entity `append` " fl r = (" `append` reqsHas
       `append` "A.fromJSON r, " `append` entityToIsField entity `append` " r, FieldListToRec fl r)\n\n"

--getAdAccount :: (R.MonadResource m, MonadBaseControl IO m, GetAdAcc fl rec) =>
--           Id -- AdAccountId    -- ^ Ad Account Id
--        -> fl     -- ^ Arguments to be passed to Facebook.
--        -> Maybe UserAccessToken -- ^ Optional user access token.
--        -> FacebookT anyAuth m rec
--getAdAccount id_ fl mtoken = getObject ("/v2.5/" <> toFbText id_) [("fields", textListToBS $ fieldNameList fl)] mtoken

-- "acc_id" and "Id" turn into:
--
-- data AccId = AccId
-- instance Field AccId where
--  type FieldValue AccId = Id
--  fieldName _ = "acc_id"
--  fieldLabel  = AccId
dataAndFieldInstance :: FieldInfo -> Text
dataAndFieldInstance (FieldInfo fieldName fieldType _ _ _) =
    let adtName = fieldNameToAdtName fieldName
    in "data "  `append` adtName `append` " = " `append` adtName `append` "\n"
        `append` "instance Field " `append` adtName `append` " where\n\t"
        `append` "type FieldValue " `append` adtName `append` " = " `append` fieldType `append` "\n\t"
        `append` "fieldName _ = \"" `append` fieldName `append` "\"\n\t"
        `append` "fieldLabel = " `append` adtName `append` "\n\n"

entityToIsField entity = "Is" `append` entity `append` "Field"

isFieldClass entity =
    let className = entityToIsField entity
    in "class " `append` className `append` " rec\n"
       `append` "instance (" `append` className `append` " h, "
       `append` className `append` " t) => " `append` className
       `append` " (h :*: t)\n"

isFieldClassInstance className instName =
    "instance " `append` className `append` " " `append` instName `append` "\n"

getterField fieldName adtName =
    fieldName `append` " rec = rec `get` " `append` adtName `append` "\n"

fieldNameToAdtName :: Text -> Text
fieldNameToAdtName str
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
