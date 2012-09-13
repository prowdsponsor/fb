{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Facebook.OpenGraph
    ( (#=)
    , SimpleType(..)
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16, Word32, Word)
import System.Locale (defaultTimeLocale)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import qualified Data.Time as TI

import Facebook.Types
import Facebook.Graph


-- | Create an 'Argument' with a 'SimpleType'.  See the docs on
-- 'createAction' for an example.
(#=) :: SimpleType a => ByteString -> a -> Argument
p #= v = (p, encodeFbParam v)


-- | Class for data types that may be represented as a Facebook
-- simple type. (see
-- <https://developers.facebook.com/docs/opengraph/simpletypes/>).
class SimpleType a where
    encodeFbParam :: a -> B.ByteString

-- | Facebook's simple type @Boolean@.
instance SimpleType Bool where
    encodeFbParam b = if b then "1" else "0"

-- | Facebook's simple type @DateTime@ with only the date.
instance SimpleType TI.Day where
    encodeFbParam = B.pack . TI.formatTime defaultTimeLocale "%Y-%m-%d"
-- | Facebook's simple type @DateTime@.
instance SimpleType TI.UTCTime where
    encodeFbParam = B.pack . TI.formatTime defaultTimeLocale "%Y%m%dT%H%MZ"
-- | Facebook's simple type @DateTime@.
instance SimpleType TI.ZonedTime where
    encodeFbParam = encodeFbParam . TI.zonedTimeToUTC

-- @Enum@ doesn't make sense to support as a Haskell data type.

-- | Facebook's simple type @Float@ with less precision than supported.
instance SimpleType Float where
    encodeFbParam = showBS
-- | Facebook's simple type @Float@.
instance SimpleType Double where
    encodeFbParam = showBS

-- | Facebook's simple type @Integer@.
instance SimpleType Int where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int8 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word8 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int16 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word16 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Int32 where
    encodeFbParam = showBS
-- | Facebook's simple type @Integer@.
instance SimpleType Word32 where
    encodeFbParam = showBS

-- | Facebook's simple type @String@.
instance SimpleType Text where
    encodeFbParam = TE.encodeUtf8
-- | Facebook's simple type @String@.
instance SimpleType ByteString where
    encodeFbParam = id

-- | An object's 'Id' code.
instance SimpleType Id where
    encodeFbParam = idCode

-- | A comma-separated list of simple types.  This definition
-- doesn't work everywhere, just for a few combinations that
-- Facebook uses (e.g. @[Int]@).  Also, encoding a list of lists
-- is the same as encoding the concatenation of all lists.  In
-- other words, this instance is here more for your convenience
-- than to make sure your code is correct.
instance SimpleType a => SimpleType [a] where
    encodeFbParam = B.concat . intersperse "," . map encodeFbParam

showBS :: Show a => a -> B.ByteString
showBS = B.pack . show
