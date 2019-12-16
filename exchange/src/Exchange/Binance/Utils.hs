module Exchange.Binance.Utils (
       subscribeToDepthBook
) where

import qualified Control.Concurrent.Chan as C
import qualified Exchange.Network.Socket as Socket
import qualified Data.Aeson as Aeson
import Exchange.Binance.Decoder
import Exchange.Types
import Finance.Types

websocketHost :: String
websocketHost = "stream.binance.com"

channels :: String
channels = "ltcusdt@depth@100ms/xrpusdt@depth@100ms/ethusdt@depth@100ms/bchusdt@depth@100ms"

subscribeToDepthBook :: C.Chan [Order] -> IO ()
subscribeToDepthBook queue = Socket.runSecureClient websocketHost ("/ws/" ++ channels) 9443 (\byteStringMsg -> C.writeChan queue $ toOrder (Aeson.decode byteStringMsg :: Maybe BinanceMessage)) (\x-> return ())