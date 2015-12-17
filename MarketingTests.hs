{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import System.Environment
import Network.HTTP.Conduit
import qualified Data.Text as T
import Facebook hiding (Id)
import qualified Facebook as FB
import Facebook.Records
import           Control.Monad.Trans.Resource
import Data.Time
import Control.Monad.Trans
import Facebook.Object.Marketing.AdAccountGen hiding (Active)
import qualified Facebook.Object.Marketing.AdAccountGen as Gen
--import Facebook.Object.Marketing.AdAccountGroupPlayground hiding (Id)
import Facebook.Object.Marketing.AdCampaign
import Facebook.Object.Marketing.AdSet
import Facebook.Object.Marketing.Utility
import Facebook.Object.Marketing.Insights

main = do
  appId <- getEnv "FB_APP_ID"
  appSecret <- getEnv "FB_APP_SECRET"
  tokenBody <- getEnv "FB_ACCESS_TOKEN"
  fbUid <- getEnv "FB_USER_ID"
  man <- newManager conduitManagerSettings
  now <- getCurrentTime
  let creds = Credentials "bdpromo" (T.pack appId) (T.pack appSecret)
      fbuser = FB.Id (T.pack fbUid)
      tokExpires = addUTCTime 1000 now
      tok = UserAccessToken fbuser (T.pack tokenBody) now
  runResourceT $ runFacebookT creds man $ do
    u <- getUser "me" [] (Just tok)
    liftIO $ print u
    Pager adaccids _ _ <- getAdAccountId tok
    liftIO $ print adaccids
    adAcc <- getAdAccount (Gen.id $ head adaccids)
                (Balance ::: AmountSpent ::: AccountId ::: Id ::: Age ::: Nil)
                (Just tok)
    liftIO $ print adAcc
    --Pager adCamps _ _ <- getAccountCampaigns (Gen.id $ head adaccids) [("fields", "name")] tok
    --liftIO $ print adCamps
    --Pager adSets _ _ <- getCampaignAdSets (acamp_id $ head adCamps) [("fields", "configured_status,effective_status,daily_budget")] tok
    --liftIO $ print adSets
    --Pager insights _ _ <- getInsights (Id $ as_id $ head adSets) [] tok
    --liftIO $ print (insights:: [WithJSON Insights])
    return ()
