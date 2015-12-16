{-# LANGUAGE BangPatterns #-}
module Facebook.Gen.Types
where

import Data.Text hiding (length)
import Data.Csv
import Control.Monad

newtype Entity = Entity Text deriving (Show, Ord, Eq)
newtype InteractionMode = InteractionMode Text deriving (Show, Ord, Eq)
newtype Boolean = Boolean Bool deriving Show
data FieldInfo = FieldInfo {
      name        :: !Text
    , type_       :: !Text
    , desc        :: !Text -- TODO: Can contain enum definitions --> change CSV files?
    , required    :: Maybe Boolean
    , maybe       :: Maybe Boolean -- when response does not contain requested field
    } deriving Show

instance Eq FieldInfo where
    -- in order to find duplicate field names for a single entity
    (==) (FieldInfo n1 _ _ _ _)
         (FieldInfo n2 _ _ _ _) = n1 == n2

instance FromField Boolean where
    parseField s
        | s == "Y" || s == "y" = pure $ Boolean True
        | otherwise = pure $ Boolean False
