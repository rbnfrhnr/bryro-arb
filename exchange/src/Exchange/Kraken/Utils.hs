module Exchange.Kraken.Utils (
       subscribeToDepthBook
) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as C
import qualified Exchange.Network.Socket as Socket
import qualified Data.ByteString.Internal as B
import qualified Data.Aeson as Aeson
import qualified Exchange.Kraken.Secured as KrakenSecured
import Control.Concurrent
import Network.WebSockets
import Exchange.Kraken.Decoder
import Exchange.Kraken.Types
import Exchange.Types
import Finance.Types

websocketHost :: String
websocketHost = "ws.kraken.com"

{- | Websocket worker which receives the order-book updates-}
subscribeToDepthBook :: C.Chan [Order] -> IO ()
subscribeToDepthBook queue = Socket.runSecureClient websocketHost "/" 443 (\byteStringMsg -> C.writeChan queue $ toOrder (Aeson.decode byteStringMsg :: Maybe KrakenMessage)) subscribe

{- | Small worker which fetches the current applicable fees in a given interval -}
subscribeToFees :: MVar.MVar KrakenFeeTable -> IO ()
subscribeToFees feeTableHolder =  do
                                  forkIO $ workerLoop
                                  return ()
                where workerLoop = do
                                   maybeFeeTable <- KrakenSecured.getKrakenFee
                                   case maybeFeeTable of
                                                 Just feeTable -> MVar.putMVar feeTableHolder feeTable
                                   threadDelay 60000000
                                   workerLoop

subscribe :: Connection -> IO ()
subscribe connection = do
                       let msg = B.packChars "{\"event\": \"subscribe\",\"pair\": [\"XRP/USD\", \"ETH/USD\", \"LTC/USD\", \"BCH/USD\"], \"subscription\": {\"name\": \"book\"}}"
                       sendTextData connection msg
                       return ()