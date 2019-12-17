{-# LANGUAGE OverloadedStrings #-}
module Exchange.Bitstamp.Secured (
       getBitstampFee
) where

import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Client as HC
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Char8 as C
import qualified Data.Time.Clock.POSIX as T
import qualified Data.UUID.V4 as V4
import qualified Data.UUID as UUID
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types.Status as ST
import Exchange.Bitstamp.Types


type APIKey = String
type ContentType = String
type Host = String
type Path = String
type Query = String
type HTTPMethod = String
type Nonce = String
type Timestamp = Int
type XAuthVersion = String
type APIKeySecret = String


apiKey :: APIKey
apiKey = ""

apiKeySecret :: APIKeySecret
apiKeySecret = ""

{- | Signing function according to https://www.bitstamp.net/api/ -}
sign :: APIKey -> HTTPMethod -> Host -> Path -> Query -> ContentType -> Nonce -> Timestamp -> XAuthVersion -> String -> String
sign apiKey method host path query contentType nonce timestamp xauthVersion body = L.unpackChars  (BL.fromStrict $ Base16.encode byteHash)
                                                                                    where byteHash = SHA256.hmac (BL.toStrict (L.packChars apiKeySecret)) byteMessage
                                                                                          byteMessage = BL.toStrict (L.packChars msg)
                                                                                          msg = "BITSTAMP " ++
                                                                                                apiKey ++
                                                                                                method ++
                                                                                                "www.bitstamp.net" ++
                                                                                                path ++
                                                                                                query ++
                                                                                                contentType ++
                                                                                                nonce ++
                                                                                                (show timestamp) ++
                                                                                                xauthVersion ++
                                                                                                body

getBitstampFee :: IO (Maybe BitstampFeeTable)
getBitstampFee = do
                request <- createRequest "/api/v2/balance/" ""
                response <- HS.httpLBS request
                return (Aeson.decode $ HC.responseBody response)

{- | creates a post request which meets the requirements posed by bitstamp-}
createRequest path payload = do
                nonce <- UUID.toString <$> V4.nextRandom
                timestamp <- (round . ( * 1000)) <$> T.getPOSIXTime
                let xAuthSignature  = sign apiKey "POST" host path "" contentType nonce timestamp xAuthVersion payload

                request <- HS.parseRequest $ "POST " ++ host

                return $ HS.setRequestPath (C.pack path)
                       $ HS.setRequestHeader "X-Auth" [C.pack xAuth]
                       $ HS.setRequestHeader "X-Auth-Nonce" [C.pack nonce]
                       $ HS.setRequestHeader "X-Auth-Signature" [C.pack xAuthSignature]
                       $ HS.setRequestHeader "X-Auth-Timestamp" [C.pack $ show timestamp]
                       $ HS.setRequestHeader "X-Auth-Version" [C.pack xAuthVersion]
                       $ request
                where xAuth           = "BITSTAMP " ++ apiKey
                      xAuthVersion    = "v2"
                      contentType     = ""
                      userAgent       = "Chrome/72.0.3626.96 Safari/537.36"
                      host            = "https://www.bitstamp.net"