module Exchange.Bitstamp.Utils (
       subscribeToDepthBook
      ,subscribeToFees
) where

import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Internal as B
import qualified Exchange.Network.Socket as Socket
import qualified Exchange.Bitstamp.Secured as BitstampSecure
import Control.Concurrent
import Finance.Types
import Exchange.Bitstamp.Decoder
import Exchange.Bitstamp.Types
import Network.WebSockets
import Exchange.Types

websocketHost :: String
websocketHost = "ws.bitstamp.net"

{- | Websocket worker which receives the order-book updates-}
subscribeToDepthBook :: C.Chan [Order] -> IO ()
subscribeToDepthBook queue = Socket.runSecureClient websocketHost "/" 443 (\byteStringMsg -> C.writeChan queue $ toOrder (Aeson.decode byteStringMsg :: Maybe BitstampMessage)) subscribe

{- | Small worker which fetches the current applicable fees in a given interval -}
subscribeToFees :: MVar.MVar BitstampFeeTable -> IO ()
subscribeToFees feeTableHolder =  do
                                  forkIO $ workerLoop
                                  return ()
                where workerLoop = do
                                   maybeFeeTable <- BitstampSecure.getBitstampFee
                                   putStrLn "updated fees for Bitstamp"
                                   case maybeFeeTable of
                                                 Just feeTable -> MVar.putMVar feeTableHolder feeTable
                                   threadDelay 60000000
                                   workerLoop

subscribe :: Connection -> IO ()
subscribe connection = do
                        let byteString = B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ltcusd\"}}"
                        sendTextData connection byteString
                        let byteString = B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ethusd\"}}"
                        sendTextData connection byteString
                        let byteString = B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_xrpusd\"}}"
                        sendTextData connection byteString
                        let byteString = B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_bchusd\"}}"
                        sendTextData connection byteString
                        return ()