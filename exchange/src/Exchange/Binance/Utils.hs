module Exchange.Binance.Utils (
       subscribeToDepthBook
      ,subscribeToFees
) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as C
import qualified Exchange.Network.Socket as Socket
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Exchange.Binance.Secured as BinanceSecured
import Control.Concurrent
import Exchange.Binance.Decoder
import Exchange.Binance.Types
import Exchange.Types
import Finance.Types
import Exchange.Network.Utils

parseBinanceMessage :: (BL.ByteString -> Either String  BinanceMessage)
parseBinanceMessage msg = (Aeson.eitherDecode msg)  :: Either String BinanceMessage

websocketHost :: String
websocketHost = "stream.binance.com"

channels :: String
channels = "ltcusdt@depth@100ms/xrpusdt@depth@100ms/ethusdt@depth@100ms/bchusdt@depth@100ms"

{- | Websocket worker which receives the order-book updates-}
subscribeToDepthBook :: C.Chan [Order] -> IO ()
subscribeToDepthBook queue = Socket.runSecureClient websocketHost ("/ws/" ++ channels) 9443 (orderFeedHandler queue parseBinanceMessage) (\x-> return ())

{- | Small worker which fetches the current applicable fees in a given interval -}
subscribeToFees :: MVar.MVar BinanceFeeTable -> IO ()
subscribeToFees feeTableHolder =  do
                                  forkIO $ workerLoop
                                  return ()
                where workerLoop = do
                                   maybeFeeTable <- BinanceSecured.getBinanceFee
                                   putStrLn "updated fees for Binance"
                                   case maybeFeeTable of
                                                 Just feeTable -> MVar.putMVar feeTableHolder feeTable
                                   threadDelay 60000000
                                   workerLoop
