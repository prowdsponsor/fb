module Facebook.Object.User
    ( User(..)
    , Gender(..)
    , getUser
    ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson ((.:), (.:?))
-- import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)

-- import qualified Control.Exception.Lifted as E
import qualified Data.Aeson as A
import qualified Data.Conduit as C
-- import qualified Data.Text as T


import Facebook.Types
import Facebook.Monad
import Facebook.Graph


-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/reference/api/user/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.
data User =
    User { userId         :: UserId
         , userName       :: Maybe Text
         , userFirstName  :: Maybe Text
         , userMiddleName :: Maybe Text
         , userLastName   :: Maybe Text
         , userGender     :: Maybe Gender
         , userLocale     :: Maybe Text
         , userUsername   :: Maybe Text
         , userVerified   :: Maybe Bool
         , userEmail      :: Maybe Text
         }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON User where
    parseJSON (A.Object v) =
      User <$> v .:  "id"
           <*> v .:? "name"
           <*> v .:? "first_name"
           <*> v .:? "middle_name"
           <*> v .:? "last_name"
           <*> v .:? "gender"
           <*> v .:? "locale"
           <*> v .:? "username"
           <*> v .:? "verified"
           <*> v .:? "email"
    parseJSON _ = mzero


-- | An user's gender.
data Gender = Male | Female deriving (Eq, Ord, Show, Read, Enum, Typeable)

instance A.FromJSON Gender where
    parseJSON (A.String "male")   = return Male
    parseJSON (A.String "female") = return Female
    parseJSON _                   = mzero

instance A.ToJSON Gender where
    toJSON = A.toJSON . toText
        where
          toText :: Gender -> Text
          toText Male   = "male"
          toText Female = "female"


-- | Get an user using his user ID.  The user access token is
-- optional, but when provided more information can be returned
-- back by Facebook.  The user ID may be @\"me\"@, in which
-- case you must provide an user access token and information
-- about the token's owner is given.
getUser :: C.ResourceIO m =>
           UserId         -- ^ User ID or @\"me\"@.
        -> [Argument]     -- ^ Arguments to be passed to Facebook.
        -> Maybe UserAccessToken -- ^ Optional user access token.
        -> FacebookT anyAuth m User
getUser id_ query mtoken = getObject ("/" <> id_) query mtoken
