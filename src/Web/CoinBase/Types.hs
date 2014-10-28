module Web.CoinBase.Types where

import           Data.ByteString     (ByteString)
import           Network.Http.Client (Method)

------------------------------------------------------------------------------
-- | Type Aliases
type Nonce = ByteString
type Url = ByteString
type Body = ByteString
type Secret = ByteString

------------------------------------------------------------------------------
-- | Connection Information
data CoinBaseConfig = CoinBaseConfig {
     publicKey :: ByteString
   , secretKey :: ByteString
   } deriving (Show)

------------------------------------------------------------------------------
-- | Coin Base Request
data CoinBaseRequest = CoinBaseRequest {
    url    :: ByteString
  , body   :: ByteString
  , method :: Method
  } deriving (Show)

------------------------------------------------------------------------------
-- | CoinBase Result
type CoinBaseResult a = Either CoinBaseError a

------------------------------------------------------------------------------
-- | Error type for coin base requests
data CoinBaseError = ConnectionError
