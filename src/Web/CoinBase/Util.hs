{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.CoinBase.Util where

import           Control.Applicative
import           Control.Monad          (mzero)
import           Crypto.Hash            (Digest, SHA256, hmac, hmacGetDigest)
import           Data.Aeson             hiding (encode)
import           Data.Byteable          (toBytes)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8  as B8
import           Data.Decimal
import qualified Data.Text              as T
import           Data.Time.Clock.POSIX

import           Web.CoinBase.Types

------------------------------------------------------------------------------
-- | Nonce Helper
getNonce :: IO ByteString
getNonce = B8.pack . show . round'  . (*1e6) <$> getPOSIXTime
  where round' :: (RealFrac a) => a -> Integer
        round' = round

------------------------------------------------------------------------------
-- | SHA256 Signature Generator
makeSignature
  :: Secret -> Nonce -> Url -> Body -> ByteString
makeSignature secret nonce uri requestBody =
     signature secret $ B.concat [
         nonce
       , uri
       , requestBody
       ]
  where signature payload key =
          encode $ toBytes (hmacGetDigest $ hmac payload key :: Digest SHA256)

------------------------------------------------------------------------------
-- | Decimal JSON Helper
instance FromJSON Decimal where
  parseJSON (String str) = return (read $ T.unpack str :: DecimalRaw Integer)
  parseJSON _ = mzero

