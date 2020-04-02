{-# LANGUAGE OverloadedStrings #-}
module Exchange.Bitstamp.Utils (
       parseBitstampMessage
      ,subscribeToFees
      ,subscribeToDepthBook
      ,subscribe
) where

import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import qualified Exchange.Network.Socket as Socket
import qualified Exchange.Bitstamp.Secured as BitstampSecure
import Exchange.Bitstamp.Contract.Websocket as BWS
import Control.Concurrent
import Finance.Types
import Exchange.Bitstamp.Types
import Network.WebSockets
import Exchange.Types
import Exchange.Network.Utils

parseBitstampMessage :: (BL.ByteString -> Either String  BWS.Message)
parseBitstampMessage msg = (Aeson.eitherDecode msg)  :: Either String BWS.Message

websocketHost :: String
websocketHost = "ws.bitstamp.net"

subscribeToDepthBook :: (BL.ByteString -> IO () ) -> IO ()
subscribeToDepthBook partialHandler = Socket.runSecureClient websocketHost "/" 443 (partialHandler) subscribe

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
subscribe connection = sendTextData connection (B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ltcusd\"}}")
                       >> (sendTextData connection (B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ethusd\"}}"))
                       >> (sendTextData connection (B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_xrpusd\"}}"))
                       >> (sendTextData connection (B.packChars "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_bchusd\"}}"))