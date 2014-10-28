{-# LANGUAGE OverloadedStrings #-}
module Web.Account where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Decimal
import           Data.Monoid
import           Data.Text           (Text)
import           Web.CoinBase.Client

------------------------------------------------------------------------------
-- | Types
data AccountBalance = AccountBalance {
    accountBalanceAmount   :: Decimal
  , accountBalanceCurrency :: Text
  } deriving (Show)

data ReceiveAddress = ReceiveAddress {
    receiveAddressSuccess     :: Bool
  , receiveAddress            :: Text
  , receiveAddressCallbackUrl :: Maybe Text
  } deriving (Show)

------------------------------------------------------------------------------
-- | JSON
instance FromJSON AccountBalance where
   parseJSON (Object o) =
     AccountBalance <$> o .: "amount"
                    <*> o .: "currency"
   parseJSON _ = mzero

instance FromJSON ReceiveAddress where
  parseJSON (Object o) =
    ReceiveAddress <$> o .: "success"
                   <*> o .: "address"
                   <*> o .:? "callback_url"
  parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Retrieve `AccountBalance`
getAccountBalance :: CoinBase AccountBalance
getAccountBalance = callAPI req 
  where req = CoinBaseRequest "account/balance" mempty GET

------------------------------------------------------------------------------
-- | Retrieve `AccountBalance`
getReceiveAddress :: CoinBase ReceiveAddress
getReceiveAddress = callAPI req
  where req = CoinBaseRequest "account/receive_address" mempty GET

