module Exchange.Kraken.Utils (
       parseKrakenMessage
      ,subscribeToDepthBook
) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as C
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import qualified Exchange.Kraken.Secured as KrakenSecured
import qualified Exchange.Kraken.Contract.Websocket as KW
import qualified Exchange.Network.Socket as Socket
import qualified System.IO as IO
import Control.Concurrent
import Network.WebSockets
import Exchange.Kraken.Decoder
import Exchange.Kraken.Types
import Exchange.Types
import Finance.Types
import Exchange.Network.Utils

parseKrakenMessage :: (BL.ByteString -> Either String  KW.KrakenMessage)
parseKrakenMessage msg = (Aeson.eitherDecode msg)  :: Either String KW.KrakenMessage

websocketHost :: String
websocketHost = "ws.kraken.com"

{- | Websocket worker which receives the order-book updates-}
subscribeToDepthBook :: (BL.ByteString -> IO () )  -> IO ()
subscribeToDepthBook partialHandler = Socket.runSecureClient websocketHost "/" 443 (partialHandler) subscribe


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
