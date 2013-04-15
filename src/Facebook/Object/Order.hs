{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module Facebook.Object.Order
	( Order(..)
	) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A

import Facebook.Types

data Order = Order {
	orderId   	:: Text,
	orderFrom 	:: UserId,
	orderTo	  	:: UserId,
	orderAmount :: Integer,
	orderStatus :: OrderStatus,
	-- orderApplication 	:: Application
	orderCountry		:: Text,
	orderRefundCode		:: Maybe Text,
	orderCreatedTime 	:: Maybe Text,
	orderUpdatedTime	:: Maybe Text
} deriving (Show)

data OrderStatus = 
	OrderPlaced 
	| OrderSettled 
	| OrderRefunded 
	| OrderDisputed 
	| OrderCancelled
	deriving (Show, Enum, Eq)

instance A.FromJSON Order where
	parseJSON (A.Object v) =
		Order <$> v .: "id"
			  <*> v .: "from"
			  <*> v .: "to"
			  <*> v .: "amount"
			  <*> v .: "status"
			  <*> v .: "country"
			  <*> v .:? "refund_reason_code"
			  <*> v .:? "created_time"
			  <*> v .:? "updated_time"
	parseJSON _ = mzero

instance A.FromJSON OrderStatus where
	parseJSON (A.String "placed") 		= return OrderPlaced
	parseJSON (A.String "settled") 		= return OrderSettled
	parseJSON (A.String "refunded")		= return OrderRefunded
	parseJSON (A.String "disputed")		= return OrderDisputed
	parseJSON (A.String "cancelled")	= return OrderCancelled
	parseJSON _							= mzero