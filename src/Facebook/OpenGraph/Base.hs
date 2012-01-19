module Facebook.OpenGraph.Base
    ( getObject
    ) where


-- import Control.Applicative
-- import Control.Monad (mzero)
-- import Data.ByteString.Char8 (ByteString)
-- import Data.Text (Text)
-- import Data.Typeable (Typeable, Typeable1)
import Network.HTTP.Types (Ascii)

-- import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Conduit as C
-- import qualified Data.Text as T
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import Facebook.Base


-- | Make a raw request to Facebook's Open Graph API.  Returns a
-- raw JSON 'A.Value'.
getObject :: C.ResourceIO m =>
             Ascii          -- ^ Path (should begin with a slash @\/@)
          -> HT.SimpleQuery -- ^ Arguments to be passed to Facebook
          -> Maybe (AccessToken kind) -- ^ Optional access token
          -> H.Manager      -- ^ HTTP connection manager.
          -> C.ResourceT m A.Value
getObject path query mtoken manager =
  asJson' =<< fbhttp (fbreq path mtoken query) manager
