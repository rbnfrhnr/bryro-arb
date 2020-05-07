module Exchange.Kraken.Utils
  ( parseKrakenMessage
  , parseToOrder
  , subscribeHandler
  , subscribeReadonly
  ) where

import qualified Control.Concurrent.Chan            as C
import qualified Control.Concurrent.MVar            as MVar
import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString.Internal           as B
import qualified Data.ByteString.Lazy               as BL
import qualified Exchange.Kraken.Contract.Websocket as KW
import qualified System.IO                          as IO
import qualified Utils.WebSocket                    as Socket

import           Control.Concurrent
import           Exchange.Types
import           Exchange.Utils
import           Finance.Types
import           Network.WebSockets

parseToOrder :: (BL.ByteString -> Either String [BaseOrder])
parseToOrder = fmap toOrder . parseKrakenMessage

parseKrakenMessage :: (BL.ByteString -> Either String KW.KrakenMessage)
parseKrakenMessage = Aeson.eitherDecode

websocketHost :: String
websocketHost = "ws.kraken.com"

{- | Websocket worker which receives the order-book updates-}
subscribeReadonly :: (BL.ByteString -> IO ()) -> IO ()
subscribeReadonly withMessage = subscribeHandler (\_ msg -> withMessage msg)

subscribeHandler :: Socket.WebsocketHandler -> IO ()
subscribeHandler handler = Socket.runSecureClient websocketHost "/" 443 handler subscribe

subscribe :: Connection -> IO ()
subscribe connection = sendTextData connection msg
  where
    msg =
      B.packChars
        "{\"event\": \"subscribe\",\"pair\": [\"XRP/USD\", \"ETH/USD\", \"LTC/USD\", \"BCH/USD\"], \"subscription\": {\"name\": \"book\"}}"
