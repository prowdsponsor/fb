{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Time (parseTime)
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


invalidCredentials :: FB.Credentials
invalidCredentials = FB.Credentials "not" "valid"

invalidUserAccessToken :: FB.AccessToken FB.User
invalidUserAccessToken = FB.UserAccessToken "invalid" farInTheFuture
    where
      Just farInTheFuture = parseTime (error "farInTheFuture") "%Y" "3000"
      -- It's actually important to use 'farInTheFuture' since we
      -- don't want any tests rejecting this invalid user access
      -- token before even giving it to Facebook.

invalidAppAccessToken :: FB.AccessToken FB.App
invalidAppAccessToken = FB.AppAccessToken "invalid"


main :: IO ()
main = H.withManager $ \manager -> liftIO $ do
  creds <- getCredentials
  hspecX $ do
    describe "getAppAccessToken" $ do
      it "works and returns a valid app access token" $ do
        token <- FB.getAppAccessToken creds manager
        valid <- FB.isValid token manager
        liftIO (valid @?= True)
      it "throws a FacebookException on invalid credentials" $ do
        ret <- E.try $ FB.getAppAccessToken invalidCredentials manager
        case ret of
          Right token                    -> fail $ show token
          Left (FB.FacebookException {}) -> return ()
    describe "isValid" $ do
      it "returns False on a clearly invalid user access token" $ do
        valid <- FB.isValid invalidUserAccessToken manager
        liftIO (valid @?= False)
      it "returns False on a clearly invalid app access token" $ do
        valid <- FB.isValid invalidAppAccessToken manager
        liftIO (valid @?= False)


-- | Sad, orphan instance.
instance (m ~ IO, r ~ ()) => Example (C.ResourceT m r) where
    evaluateExample = evaluateExample . C.runResourceT
