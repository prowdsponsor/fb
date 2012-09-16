module TryIt (runFB) where

-- You may import this file from GHCi in order to try the fb
-- package.  For example:
--
--   > :l tryIt
--
--   > runFB FB.getAppAccessToken

import qualified Data.Conduit as C
import qualified Facebook as FB
import qualified Network.HTTP.Conduit as H
import Main (getCredentials)

runFB :: FB.FacebookT FB.Auth (C.ResourceT IO) a -> IO a
runFB act = do
  creds <- getCredentials
  H.withManager $ \m -> FB.runFacebookT creds m act
