module Exchange.Kraken
  ( KrakenHandle
  , new
  ) where

import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString.Internal           as B
import qualified Data.ByteString.Lazy               as BL
import qualified Exchange.Kraken.Contract.Websocket as KW
import qualified Utils.WebSocket                    as Socket

import           Exchange.Types
import           Finance
import           Network.WebSockets
import           System.IO                          (hFlush, stdout)

data KrakenHandle =
  KrakenHandle
    { krakenHandleWsConfig          :: !WebsocketConfig
    , krakenHandleOrderBookEndpoint :: String
    }

new :: KrakenHandle
new = KrakenHandle wsconfig ""
  where
    wsconfig = WebsocketConfig 443 "/" "ws.kraken.com"

instance ExchangeAdapter KrakenHandle where
  subscribeOrderBook krakenHandle handler = subscribeHandler krakenHandle handler

subscribeHandler :: KrakenHandle -> ([BaseOrder] -> IO ()) -> IO ()
subscribeHandler (KrakenHandle wsConfig _) handler =
  Socket.new
    host
    path
    port
    (\con msg -> either (\err -> print err >> hFlush stdout) handler (parseToOrder msg))
    subscribe
  where
    path = websocketPath wsConfig
    port = websocketPort wsConfig
    host = websocketHost wsConfig

subscribe :: Connection -> IO ()
subscribe connection = sendTextData connection msg
  where
    msg =
      B.packChars
        "{\"event\": \"subscribe\",\"pair\": [\"XRP/USD\", \"ETH/USD\", \"LTC/USD\", \"BCH/USD\"], \"subscription\": {\"name\": \"book\"}}"

parseToOrder :: (BL.ByteString -> Either String [BaseOrder])
parseToOrder = fmap toOrder . parseKrakenMessage

parseKrakenMessage :: (BL.ByteString -> Either String KW.KrakenMessage)
parseKrakenMessage = Aeson.eitherDecode
