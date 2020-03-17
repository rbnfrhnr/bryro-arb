module Exchange.Kraken.Utils (
       subscribeToDepthBook
) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as C
import qualified Exchange.Network.Socket as Socket
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import qualified Exchange.Kraken.Secured as KrakenSecured
import qualified Exchange.Kraken.Contract.Websocket as KW
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
subscribeToDepthBook queue = Socket.runSecureClient websocketHost "/" 443 (handleKrakenMessage queue) subscribe

handleKrakenMessage :: C.Chan [Order] -> BL.ByteString -> IO ()
handleKrakenMessage queue byteStringMsg = case (Aeson.decode byteStringMsg :: Maybe KW.KrakenOrderMessage) of
                                            Just msg -> C.writeChan queue $ toOrder msg
                                            Nothing -> putStrLn $ "Unknown Bitstamp message !\n \t Message received: " ++ (show byteStringMsg)


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