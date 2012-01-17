{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.IO.Error (isDoesNotExistError)

import qualified Data.ByteString.Char8 as B
import qualified Control.Exception.Lifted as E
import qualified Data.Conduit as C
import qualified Facebook as FB
import qualified Network.HTTP.Conduit as H

import Test.HUnit
import Test.Hspec.Core (Example(..))
import Test.Hspec.Monadic
-- import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()


-- | Grab the Facebook credentials from the environment.
getCredentials :: IO FB.Credentials
getCredentials = tryToGet `E.catch` showHelp
    where
      tryToGet = do
        [appId, appSecret] <- mapM getEnv ["APP_ID", "APP_SECRET"]
        return $ FB.Credentials (B.pack appId) (B.pack appSecret)

      showHelp exc | not (isDoesNotExistError exc) = E.throw exc
      showHelp _ = do
        putStrLn $ unlines
          [ "In order to run the tests from the 'fb' package, you need"
          , "developer access to a Facebook app.  The tests are designed"
          , "so that your app isn't going to be hurt, but we may not"
          , "create a Facebook app for this purpose and then distribute"
          , "its secret keys in the open."
          , ""
          , "Please give your app id and app secret on the enviroment"
          , "variables APP_ID and APP_SECRET, respectively.  For example,"
          , "before running the test you could run in the shell:"
          , ""
          , "  $ export APP_ID=\"458798571203498\""
          , "  $ export APP_SECRET=\"28a9d0fa4272a14a9287f423f90a48f2304\""
          , ""
          , "Of course, the values above aren't valid and you need to"
          , "replace them with your own."
          , ""
          , "(Exiting now with a failure code.)"]
        exitFailure


main :: IO ()
main = H.withManager $ \manager -> liftIO $ do
  creds <- getCredentials
  hspecX $ do
    describe "getAppAccessToken" $ do
      it "works and returns a valid appaccess token" $ do
        token <- FB.getAppAccessToken creds manager
        valid <- FB.isValid token manager
        liftIO (valid @?= True)
    describe "isValid" $ do
      it "returns False on a clearly invalid access token" $ do
        let token = FB.AccessToken "not valid" Nothing
        valid <- FB.isValid token manager
        liftIO (valid @?= False)


-- | Sad, orphan instance.
instance (m ~ IO, r ~ ()) => Example (C.ResourceT m r) where
    evaluateExample = evaluateExample . C.runResourceT
