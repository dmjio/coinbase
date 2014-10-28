{-# LANGUAGE OverloadedStrings #-}
module Web.AccountChanges where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Decimal
import           Data.Monoid
import           Data.Text           (Text)
import           Web.CoinBase.Client

------------------------------------------------------------------------------
-- | Types
data AccountChanges = AccountChanges {
    accountChangesAmount   :: Decimal
  , accountChangesCurrency :: Text
  } deriving (Show)
------------------------------------------------------------------------------
-- | JSON
instance FromJSON AccountChanges where
   parseJSON (Object o) =
     AccountChanges <$> o .: "amount"
                    <*> o .: "currency"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Retrieve `AccountChanges`
getAccountChanges :: CoinBase AccountChanges
getAccountChanges = callAPI req 
  where req = CoinBaseRequest "account/changes" mempty GET
