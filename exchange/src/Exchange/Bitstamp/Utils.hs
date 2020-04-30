{-# LANGUAGE OverloadedStrings #-}

module Exchange.Bitstamp.Utils
  ( parseBitstampMessage
  , subscribeToDepthBook
  ) where

import qualified Control.Concurrent.Chan              as C
import qualified Control.Concurrent.MVar              as MVar
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Internal             as B
import qualified Data.ByteString.Lazy                 as BL
import qualified Utils.WebSocket                      as Socket

import           Control.Concurrent
import           Exchange.Bitstamp.Contract.Websocket as BWS
import           Exchange.Utils
import           Exchange.Types
import           Finance.Types
import           Network.WebSockets

orderChannelsToSubscribe :: [String]
orderChannelsToSubscribe =
  [ "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ltcusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ethusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_xrpusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_bchusd\"}}"
  ]

parseBitstampMessage :: (BL.ByteString -> Either String BWS.BitstampMessage)
parseBitstampMessage msg = Aeson.eitherDecode msg :: Either String BWS.BitstampMessage

websocketHost :: String
websocketHost = "ws.bitstamp.net"

subscribeToDepthBook :: (BL.ByteString -> IO ()) -> IO ()
subscribeToDepthBook partialHandler = Socket.runSecureClient websocketHost "/" 443 partialHandler subscribe

subscribe :: Connection -> IO ()
subscribe connection = foldl (\_ cts -> sendTextData connection (B.packChars cts)) (pure ()) orderChannelsToSubscribe
