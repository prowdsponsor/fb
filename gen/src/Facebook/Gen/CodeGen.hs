module Facebook.Gen.CodeGen
where

import Language.Haskell.Generate
import Data.Text
import qualified Data.Text as T

import Facebook.Gen.Environment

imports = ["Facebook.Record"]

envToCode :: Env -> [ModuleG]
envToCode (Env env) = undefined

--testMod = 
--    let mod = runModuleM testCode "TestModule"
testCode = do
      d <- addDecl (Ident "main") $ applyE  putStrLn' $ expr ("Hello World" :: String)
      e <- addDecl (Ident "main") $ applyE  putStrLn' $ expr ("Hello World" :: String)
      return $ Just [exportFun d, exportFun e]

--genAdtAndFieldInstance :: Text -> ...

-- "acc_id" turns into:
--
-- data AccId = AccId
fieldToADT :: Name -> Decl
fieldToADT name = 
    let context = []
        tyVar = []
        typeInf = []
        constr = [QualConDecl srcLocC tyVar context $ ConDecl name typeInf]
        derive = []
        srcLocD = SrcLoc "FileName" 0 0 -- works fine...
        srcLocC = SrcLoc "FileName" 0 0
    in DataDecl srcLocD DataType context name tyVar constr derive

-- turns "acc_id", AccId, and Id into:
--
-- instance Field AccId where
--  type FieldValue AccId = Id
--  fieldName _ = "acc_id"
--  fieldLabel  = AccId
fieldToFieldInstance :: Name -> Name -> Name -> Decl -- ???
fieldToFieldInstance fieldName adtName fieldType = 
    let overlap = Nothing
        tyVar = [UnkindedVar adtName]
        context = []
        qname = UnQual $ Ident "Field"
        typee = [] -- ???
        insType = InsType srcLoc (TyApp (TyVar $ Ident "FieldValue") (TyVar adtName)) $ TyVar fieldType
        --fieldNameDecl = InsDecl 
        --decls = [insType, fieldNameDecl, fieldLabelDecl]
        srcLoc = SrcLoc "FileName" 0 0
    in InstDecl srcLoc overlap tyVar context qname typee [insType]

fieldNameToADTName :: Text -> String
fieldNameToADTName str
    | T.null $ dropUnderscore str = unpack str
    | otherwise =
        let str' = dropUnderscore str
            first = charToUpper $ T.head str'
            camel = toCamelCase "" $ T.tail str'
        in unpack $ first `append` camel
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
