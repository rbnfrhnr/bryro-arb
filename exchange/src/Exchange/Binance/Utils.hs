module Exchange.Binance.Utils
  ( parseBinanceMessage
  , subscribeReadonly
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
import           Finance.Types

parseBinanceMessage :: (BL.ByteString -> Either String BinanceMessage)
parseBinanceMessage msg = Aeson.eitherDecode msg :: Either String BinanceMessage

websocketHost :: String
websocketHost = "stream.binance.com"

channels :: String
channels = "ltcusdt@depth@100ms/xrpusdt@depth@100ms/ethusdt@depth@100ms/bchusdt@depth@100ms"

{- | Websocket worker which receives the order-book updates. readonly and no way to interfere with the connection -}
subscribeReadonly :: (BL.ByteString -> IO ()) -> IO ()
subscribeReadonly withMessage = subscribeHandler (\_ msg -> withMessage msg)

{- | Websocket worker which allows to interact with the exchange -}
subscribeHandler :: Socket.WebsocketMessageHandler -> IO ()
subscribeHandler handler = Socket.runSecureClient websocketHost ("/ws/" ++ channels) 9443 handler (\x -> return ())
