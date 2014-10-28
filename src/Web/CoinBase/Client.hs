{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.CoinBase.Client
       ( -- * API
         coinBase
       , sendCoinBaseRequest
       , callAPI
         -- * Types
       , CoinBase
       , module Web.CoinBase.Types
       , module Web.CoinBase.Util
       , Method (..)
       )
       where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader
import           Data.Aeson                 hiding (encode)
import           Data.Monoid
import           Network.Http.Client
import           OpenSSL

import           Web.CoinBase.Types
import           Web.CoinBase.Util

------------------------------------------------------------------------------
-- | Core Type
type CoinBase a = ReaderT CoinBaseConfig IO a

------------------------------------------------------------------------------
-- | CoinBase API request method
coinBase
  :: FromJSON a
  => CoinBaseConfig 
  -> CoinBase a
  -> IO a
coinBase = flip runReaderT
 
------------------------------------------------------------------------------
-- | API request to be issued
callAPI
  :: (Show a, FromJSON a)
  => CoinBaseRequest
  -> CoinBase a
callAPI request = ask >>= \config -> 
  liftIO $ sendCoinBaseRequest config request

------------------------------------------------------------------------------
-- | Request Builder for CoinBase
sendCoinBaseRequest
  :: (Show a, FromJSON a)
  => CoinBaseConfig
  -> CoinBaseRequest
  -> IO a
sendCoinBaseRequest
  CoinBaseConfig  {..}
  CoinBaseRequest {..} = 
   withOpenSSL $ do
      ctx <- baselineContextSSL
      nonce <- getNonce
      con <- openConnectionSSL ctx "api.coinbase.com" 443
      let urlPrefix = "https://api.coinbase.com/v1/"
          fullUrl   = urlPrefix <> url
          sig       = makeSignature secretKey nonce fullUrl body
      req <- buildRequest $ do
        http method $ "/v1/" <> url
        setHeader "Connection" "Keep-Alive"
        setHeader "User-Agent" "Haskell"
        setHeader "ACCESS_NONCE" nonce
        setHeader "ACCESS_KEY" publicKey
        setHeader "ACCESS_SIGNATURE" sig
        setContentType "application/json"
        setAccept "*/*"
      sendRequest con req emptyBody
      response <- receiveResponse con jsonHandler
      print response
      closeConnection con
      return response




