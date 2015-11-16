{-# LANGUAGE FlexibleContexts #-}

module Facebook.Object.Marketing.GettingStarted where
import Facebook
import Control.Applicative
import Control.Monad.Trans.Resource
import Facebook.Object.Marketing.AdUser
import Facebook.Object.Marketing.AdAccount
import Facebook.Object.Marketing.AdImage
import Facebook.Object.Marketing.AdCreative
import Facebook.Object.Marketing.AdAccountGroup
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.AdSet as AS
import Facebook.Object.Marketing.Ad
import Facebook.Object.Marketing.AdCampaign as ACAMP
import Facebook.Object.Marketing.TargetingSpecs
import Data.Default
import           Network.HTTP.Conduit

gettingStarted :: (MonadResource m, Monad m, Applicative m, MonadBaseControl IO m) => FacebookT auth m ()
--gettingStarted = undefined
gettingStarted = do
   myAccId <- id <$> getAdAccountId undefined
   let myCampaign = AdCampaign
         { acamp_name = Just "Test facebook campaign"
         , acamp_campaign_group_status = Just ACAMP.PAUSED
         , acamp_objective = undefined --Just "To test the Haskell Facebook Api"
         }
   myCampaignId <- oc_id <$> createCamapign myCampaign
   let myAdset = AdSet
         { as_name= Just "My Ad Set"
         , as_configured_status  = Just ACAMP.PAUSED
         , as_daily_budget = Just $ Money 5
         , as_bid_amount = Just $ Money 1
         , as_billing_event = undefined
         , as_optimization_goal = undefined
         , as_targeting = undefined
         , as_campaign_group_id = undefined myCampaignId
         }
   myAdsetId <- createAdSet myAdset
   myCreative <- createCreative
   let sampleAd = Ad { a_name = Just "MyAd"
                     , a_campaign_id = undefined
                     , a_creative = undefined
                     }
   _ <- createAd sampleAd
   return ()
   where
     getAdAccount  = undefined
     createCamapign = undefined
     createAdSet = undefined
     createCreative = undefined
     createAd = undefined
