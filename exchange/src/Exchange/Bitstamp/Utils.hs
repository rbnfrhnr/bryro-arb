{-# LANGUAGE OverloadedStrings #-}

module Exchange.Bitstamp.Utils
  ( parseBitstampMessage
  , parseToOrder
  , subscribeHandler
  , subscribeReadonly
  ) where

import qualified Control.Concurrent.Chan              as C
import qualified Control.Concurrent.MVar              as MVar
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Internal             as B
import qualified Data.ByteString.Lazy                 as BL
import qualified Utils.WebSocket                      as Socket

import           Control.Concurrent
import           Control.Monad                        (foldM)
import           Exchange.Bitstamp.Contract.Websocket as BWS
import           Exchange.Types
import           Exchange.Utils
import           Finance
import           Network.WebSockets

orderChannelsToSubscribe :: [String]
orderChannelsToSubscribe =
  [ "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ltcusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_ethusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_xrpusd\"}}"
  , "{\"event\": \"bts:subscribe\",\"data\": {\"channel\": \"diff_order_book_bchusd\"}}"
  ]

parseToOrder :: (BL.ByteString -> Either String [BaseOrder])
parseToOrder = fmap toOrder . parseBitstampMessage

parseBitstampMessage :: (BL.ByteString -> Either String BitstampMessage)
parseBitstampMessage = Aeson.eitherDecode

websocketHost :: String
websocketHost = "ws.bitstamp.net"

subscribeReadonly :: (BL.ByteString -> IO ()) -> IO ()
subscribeReadonly withMessage = subscribeHandler (\_ msg -> withMessage msg)

subscribeHandler :: Socket.WebsocketHandler -> IO ()
subscribeHandler handler = Socket.new websocketHost "/" 443 handler subscribe >>= Socket.run

subscribe :: Connection -> IO ()
subscribe connection = foldM (\_ cts -> sendTextData connection (B.packChars cts)) () orderChannelsToSubscribe
