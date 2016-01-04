{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import System.Environment
import Network.HTTP.Conduit
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Facebook hiding (Id)
import qualified Facebook as FB
import Facebook.Records
import           Control.Monad.Trans.Resource
import Data.Time
import Control.Monad.Trans
import Facebook.Object.Marketing.AdAccount
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.AdCampaign
import Facebook.Object.Marketing.AdSet
import Facebook.Object.Marketing.AdImage
import Facebook.Object.Marketing.Utility hiding (toBS)
import Facebook.Object.Marketing.Insights
import Prelude hiding (id)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Codec.Picture

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
    Pager adaccids _ _ <- getAdAccountId $ Just tok
    liftIO $ print adaccids
    adAcc <- getAdAccount (id $ head adaccids)
                (Balance ::: AmountSpent ::: Age ::: Nil)
                (Just tok)
    liftIO $ print adAcc
    Pager adCamps _ _ <- getAdCampaign (id $ head adaccids) (Name ::: Nil) tok
    liftIO $ print adCamps
    Pager adSets _ _ <- getAdSet (id $ head adCamps) (ConfiguredStatus ::: EffectiveStatus ::: DailyBudget ::: Nil) tok
    liftIO $ print adSets
    let (Id_ idText) = id $ head adSets
    Pager insights _ _ <- getInsights (FB.Id idText) [] tok
    liftIO $ print (insights:: [WithJSON Insights])
    Pager images _ _ <- getAdImage (id $ head adaccids) 
        (Id ::: Nil) tok
    liftIO $ print images
    liftIO $ print $ length images
    --(Right (ImageYCbCr8 img)) <- liftIO $ readJpeg "bridge.jpg"
    --let bs = encodeJpegAtQuality 100 img
    bs <- liftIO $ BS8.readFile "bridge.jpg"
    let rec = (Bytes, Bytes_ bs) :*: Nil
    --liftIO $ BS.writeFile "bridge2.jpg" $ BSL.toStrict bs
    --liftIO $ print bs
    --liftIO $ print $ toParam rec
    adImg <- setAdImage (id $ head adaccids) rec tok
    liftIO $ print adImg
    Pager images' _ _ <- getAdImage (id $ head adaccids) 
        (Id ::: Nil) tok
    liftIO $ print images'
    liftIO $ print $ length images'
    -- TODO: Upload Image, create Ad; test here; then beautilitics
    return ()
