module Exchange.Bitstamp.Utils (
       subscribeToDepthBook
      ,subscribeToFees
) where

import qualified Control.Concurrent.Chan as C
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Internal as B
import qualified Exchange.Network.Socket as Socket
import Finance.Types
import Exchange.Bitstamp.Decoder
import Network.WebSockets
import Exchange.Types

websocketHost :: String
websocketHost = "ws.bitstamp.net"

subscribeToDepthBook :: C.Chan [Order] -> IO ()
subscribeToDepthBook queue = Socket.runSecureClient websocketHost "/" 443 (\byteStringMsg -> C.writeChan queue $ toOrder (Aeson.decode byteStringMsg :: Maybe BitstampMessage)) subscribe

subscribeToFees :: IO ()
subscribeToFees = putStrLn "SubscribeToFee"

getBalance :: IO ()
getBalance = putStrLn "fetch balance"

placeOrder :: IO ()
placeOrder = putStrLn "place Order"

cancelOrder :: IO ()
cancelOrder = putStrLn "cancel Order"


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