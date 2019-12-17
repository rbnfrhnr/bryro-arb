{-# LANGUAGE OverloadedStrings #-}
module Exchange.Binance.Secured (
       getBinanceFee
) where

import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BS
import qualified Data.Time.Clock.POSIX as T
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types.Status as ST
import Exchange.Binance.Types


type APIKey = String
type APIKeySecret = String


apiKey :: APIKey
apiKey = ""

apiKeySecret :: APIKeySecret
apiKeySecret = ""

{- | Signing function according to https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#signed-endpoint-examples-for-post-apiv3order -}
sign :: APIKey -> String -> String
sign apiKey body = L.unpackChars  (BL.fromStrict $ Base16.encode byteHash)
             where byteHash = SHA256.hmac (BL.toStrict (L.packChars apiKeySecret)) byteMessage
                   byteMessage = BL.toStrict (L.packChars body)

getBinanceFee :: IO (Maybe BinanceFeeTable)
getBinanceFee = do
                 request <- createRequest "/api/v3/account" ""
                 response <- HS.httpLBS request
                 return (Aeson.decode $ HC.responseBody response)

{- | Creates a get Request for Binance. Get Request require the payload to be passed as query parameters -}
createRequest path payload = do
                timestamp <- (round . ( * 1000)) <$> T.getPOSIXTime
                let body = payload ++ "timestamp=" ++ (show timestamp)
                let signature  = sign apiKey body

                request <- HS.parseRequest $ "GET " ++ host
                return $  HS.setRequestPath (C.pack (path ++ "?" ++ body ++ "&signature=" ++ signature ))
                          $ HS.setRequestHeader "X-MBX-APIKEY" [C.pack apiKey]
                          $ request
                where host            = "https://api.binance.com"