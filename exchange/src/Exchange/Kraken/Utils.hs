module Exchange.Kraken.Utils (
       subscribeToDepthBook
) where

import qualified Control.Concurrent.Chan as C
import qualified Exchange.Network.Socket as Socket
import qualified Data.ByteString.Internal as B
import qualified Data.Aeson as Aeson
import Network.WebSockets
import Exchange.Kraken.Decoder
import Exchange.Types
import Finance.Types

websocketHost :: String
websocketHost = "ws.kraken.com"

subscribeToDepthBook :: C.Chan [Order] -> IO ()
subscribeToDepthBook queue = Socket.runSecureClient websocketHost "/" 443 (\byteStringMsg -> C.writeChan queue $ toOrder (Aeson.decode byteStringMsg :: Maybe KrakenMessage)) subscribe


subscribe :: Connection -> IO ()
subscribe connection = do
                       let msg = B.packChars "{\"event\": \"subscribe\",\"pair\": [\"XRP/USD\", \"ETH/USD\", \"LTC/USD\", \"BCH/USD\"], \"subscription\": {\"name\": \"book\"}}"
                       sendTextData connection msg
                       return ()