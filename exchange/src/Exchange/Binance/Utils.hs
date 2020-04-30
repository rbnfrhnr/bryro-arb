module Exchange.Binance.Utils
  ( parseBinanceMessage
  , subscribeToDepthBook
  , subscribeToFees
  ) where

import qualified Control.Concurrent.Chan  as C
import qualified Control.Concurrent.MVar  as MVar
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Lazy     as BL
import qualified Exchange.Binance.Secured as BinanceSecured
import qualified Utils.WebSocket          as Socket

import           Control.Concurrent
import           Exchange.Binance.Decoder
import           Exchange.Binance.Types
import           Exchange.Network.Utils
import           Exchange.Types
import           Finance.Types

parseBinanceMessage :: (BL.ByteString -> Either String BinanceMessage)
parseBinanceMessage msg = Aeson.eitherDecode msg :: Either String BinanceMessage

websocketHost :: String
websocketHost = "stream.binance.com"

channels :: String
channels = "ltcusdt@depth@100ms/xrpusdt@depth@100ms/ethusdt@depth@100ms/bchusdt@depth@100ms"

{- | Websocket worker which receives the order-book updates-}
subscribeToDepthBook :: (BL.ByteString -> IO ()) -> IO ()
subscribeToDepthBook partialHandler =
  Socket.runSecureClient websocketHost ("/ws/" ++ channels) 9443 partialHandler (\x -> return ())

{- | Small worker which fetches the current applicable fees in a given interval -}
subscribeToFees :: MVar.MVar BinanceFeeTable -> IO ()
subscribeToFees feeTableHolder = forkIO (fetchFees feeTableHolder) >> return ()

fetchFees :: MVar.MVar BinanceFeeTable -> IO ()
fetchFees feeTableHolder = do
  maybeFeeTable <- BinanceSecured.getBinanceFee
  case maybeFeeTable of
    Just feeTable -> MVar.putMVar feeTableHolder feeTable
  threadDelay $ 60 * 1000 * 1000
