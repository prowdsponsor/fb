{-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs, InstanceSigs, OverloadedStrings, FlexibleContexts, OverlappingInstances, UndecidableInstances, FunctionalDependencies #-}

module Facebook.Records where

import Data.Aeson
import Data.Aeson.Types
import Data.Text hiding (foldr)
import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map
import Prelude hiding (take, length)

data Nil = Nil

infixr 5 :*:

data f :*: b where
  (:*:) :: Field f => (f, FieldValue f) -> b -> f :*: b

class Field a where
  type FieldValue a
  fieldName :: a -> Text
  fieldLabel :: a

instance forall a b. (Show (FieldValue a), Show b) => Show (a :*: b) where
    show ((f, v) :*: rest) = (unpack $ fieldName f) ++ ": " ++ show v ++ "\n" ++ show rest
instance Show Nil where
    show _ = ""

-- in order to feed getObject in Facebook/Graph.hs
class ToBS a where
    toBS :: a -> BS.ByteString

instance ToBS Nil where
    toBS _ = ""

instance (ToBS a, Field f) => ToBS (f :*: a) where
    toBS ((f, _) :*: rest) =
        let str = toBS rest
        in if BS.null str -- TODO: Use Builder
            then fieldToByteString f
            else fieldToByteString f `BS.append` "," `BS.append` str

fieldToByteString :: Field f => f -> BS.ByteString
fieldToByteString f = encodeUtf8 $ fieldName f

instance FromJSON Nil where
  parseJSON _ = return Nil

instance (FromJSON a, Field f, FromJSON (FieldValue f)) => FromJSON (f :*: a) where
  parseJSON = parseJSONRec

parseJSONRec :: forall f a. (FromJSON a, Field f, FromJSON (FieldValue f)) => Value -> Parser (f:*:a)
parseJSONRec o@(Object v) = do
    let flabel = fieldLabel :: f
    v <- v .: fieldName flabel
    rest <- parseJSON o
    return $ (flabel, v) :*: rest
parseJSONRec _ = fail "Parameter to parseJSONRec not of type Object"

class Field f => Has f r where
  get :: r -> f -> FieldValue f

instance Field f => Has f (f :*: a) where
  get ((_, v) :*: _) f = v

instance (Field f, Has f r) => Has f (g :*: r) where
  get (_ :*: r) f = get r f

instance ToJSON Nil where
  toJSON _ = emptyObject

instance (ToJSON a, Field f, ToJSON (FieldValue f)) => ToJSON (f :*: a) where
  toJSON = toJSONRec

toJSONRec :: forall f a. (ToJSON a, Field f, ToJSON (FieldValue f)) => (f :*: a) -> Value
toJSONRec ((f, v) :*: rest) =
    let curMap = Map.singleton (fieldName f) $ toJSON v
    in case toJSON rest of -- will always be Object since we are representing records
        Object hmap -> toJSON $ Map.union curMap hmap


-- List-level concatenation of Fields

infixr 5 :::

data f ::: b where
  (:::) :: Field f => f -> b -> f ::: b

class FieldListToRec l r | l -> r where
  fieldNameList :: l -> [Text]

instance FieldListToRec Nil Nil where
  fieldNameList _ = []

instance (FieldListToRec l r, Field f) => FieldListToRec (f ::: l) (f :*: r) where
  fieldNameList (fld:::flds) = fieldName fld : fieldNameList flds

textListToBS :: [Text] -> BS.ByteString
textListToBS xs =
    let xs' = foldr (\a b -> a `append` "," `append` b) "" xs
        n = length xs'
    in encodeUtf8 $ take (n - 1) xs' -- drop last comma
