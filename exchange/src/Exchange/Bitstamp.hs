module Exchange.Bitstamp
  ( BitstampHandle
  , new
  ) where

import qualified Control.Concurrent.Chan              as C
import qualified Control.Concurrent.MVar              as MVar
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Internal             as B
import qualified Data.ByteString.Lazy                 as BL
import           Exchange.Types
import qualified Utils.WebSocket                      as Socket

import           Control.Concurrent
import           Control.Monad                        (foldM)
import           Exchange.Bitstamp.Contract.Websocket as BWS
import           Exchange.Types
import           Exchange.Utils
import           Finance
import           Network.WebSockets
import           System.IO                            (hFlush, stdout)

data BitstampHandle =
  BitstampHandle
    { bitstampHandleWsConfig :: !WebsocketConfig
    }

instance ExchangeAdapter BitstampHandle where
  subscribeOrderBook (BitstampHandle wsConfig) handler =
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

new :: BitstampHandle
new = BitstampHandle (WebsocketConfig 443 "/" "ws.bitstamp.net")

orderChannelsToSubscribe :: [String]
orderChannelsToSubscribe =
  [ "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ltcusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ethusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_xrpusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_bchusd\"}}"
  ]

parseToOrder :: (BL.ByteString -> Either String [BaseOrder])
parseToOrder = fmap toOrder . parseBitstampMessage

parseBitstampMessage :: (BL.ByteString -> Either String BitstampMessage)
parseBitstampMessage = Aeson.eitherDecode

subscribe :: Connection -> IO ()
subscribe connection = foldM (\_ cts -> sendTextData connection (B.packChars cts)) () orderChannelsToSubscribe
