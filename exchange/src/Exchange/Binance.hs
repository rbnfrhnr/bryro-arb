module Exchange.Binance
  ( BinanceHandle
  , new
  ) where

import qualified Control.Concurrent.Chan             as C
import qualified Control.Concurrent.MVar             as MVar
import qualified Data.Aeson                          as Aeson
import qualified Data.ByteString.Lazy                as BL
import qualified Utils.WebSocket                     as Socket

import           Control.Concurrent
import           Exchange.Binance.Contract.Websocket
import           Exchange.Types
import           Exchange.Utils
import           Finance
import           System.IO                           (hFlush, stdout)

data BinanceHandle =
  BinanceHandle
    { binanceHandleWsConfig          :: !WebsocketConfig
    , binanceHandleOrderBookChannels :: !String
    }

instance ExchangeAdapter BinanceHandle where
  subscribeOrderBook (BinanceHandle wsConfig channels) handler =
    Socket.new
      host
      (path ++ "/" ++ channels)
      port
      (\con msg -> either (\err -> print err >> hFlush stdout) handler (parseToOrder msg))
      (\x -> return ())
    where
      path = websocketPath wsConfig
      port = websocketPort wsConfig
      host = websocketHost wsConfig

new :: BinanceHandle
new =
  BinanceHandle
    (WebsocketConfig 443 "/ws" "stream.binance.com")
    "ltcusdt@depth@100ms/xrpusdt@depth@100ms/ethusdt@depth@100ms/bchusdt@depth@100ms"

parseToOrder :: (BL.ByteString -> Either String [BaseOrder])
parseToOrder = fmap toOrder . parseBinanceMessage

parseBinanceMessage :: (BL.ByteString -> Either String BinanceMessage)
parseBinanceMessage = Aeson.eitherDecode
