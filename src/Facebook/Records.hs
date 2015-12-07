{-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs, InstanceSigs, OverloadedStrings, FlexibleContexts, OverlappingInstances #-}

module Facebook.Records where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
data Nil = Nil

infixr 5 :*:

data f :*: b where
  (:*:) :: Field f => (f, FieldValue f) -> b -> f :*: b

class Field a where
  type FieldValue a
  fieldName :: a -> Text
  fieldLabel :: a

data Name = Name

instance Field Name where
  type FieldValue Name = String
  fieldName _ = "name"
  fieldLabel = Name

data Age = Age

instance Field Age where
  type FieldValue Age = Int
  fieldName _ = "age"
  fieldLabel = Age

myrec :: Name :*: Age :*: Nil
myrec = (Name, "Tom") :*: (Age, 38) :*: Nil

tomage = myrec `get` Age
tomname = myrec `get` Name

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

class Field f => Has f r where
  get :: r -> f -> FieldValue f

instance Field f => Has f (f :*: a) where
  get ((_, v) :*: _) f = v

instance (Field f, Has f r) => Has f (g :*: r) where
  get (_ :*: r) f = get r f

instance ToJSON Nil where
  toJSON _ = toJSON ()

--instance ToJSON ....
