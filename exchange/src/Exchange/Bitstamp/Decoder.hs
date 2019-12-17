{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Exchange.Bitstamp.Decoder (
       BitstampMessage(..)
) where

import Data.Aeson
import GHC.Generics
import Finance.Types
import Exchange.Types

{- | actual payload from an orderbook update event from bitstamp. -}
data Data = Data{
    asks           :: ![[Double]] -- ^ array which represents a tuple. fst: price, snd: qty
   ,bids           :: ![[Double]] -- ^ array which represents a tuple. fst: price, snd: qty
   ,timestamp      :: !Int        -- ^ timestamp of the event (bitstamp server time)
   ,microtimestamp :: !Int        -- ^ timestamp in microseconds (bitstamp server time)
} deriving (Show, Generic)

instance FromJSON Data
      where parseJSON (Object v) = do
                                   asks <- v .:? "asks" .!= []
                                   bids <- v .:? "bids" .!= []
                                   timestamp <- v .:? "timestamp" .!= "0"
                                   microtimestamp <- v .:? "microtimestamp" .!= "0"
                                   return (Data { asks = map (map read) asks :: [[Double]],
                                                   bids = map (map read ) bids :: [[Double]],
                                                   timestamp = read timestamp :: Int,
                                                   microtimestamp = read microtimestamp :: Int})
instance ToJSON Data

data BitstampMessage = BitstampMessage {
  channel :: String,
  event   :: String,
  msg    :: Data
} deriving (Show, Generic)

instance FromJSON BitstampMessage
    where parseJSON (Object v) = do
                                 channel <- v .: "channel"
                                 event <- v .: "event"
                                 msg <- v .: "data"
                                 return (BitstampMessage { channel = channel, event = event, msg = msg })

instance ToJSON BitstampMessage

instance ExchangeOrder BitstampMessage where
  toOrder message = (map (\(price:qty:[]) -> AskOrder (BaseOrder Bitstamp currencyPair price qty msgTimestamp)) asksArray) ++ (map (\(price:qty:[]) -> BidOrder (BaseOrder Bitstamp currencyPair price qty msgTimestamp)) bidsArray)
                where msgTimestamp = timestamp (msg message)
                      currencyPair = getCurrencyPairFromMessage message
                      asksArray    = asks (msg message)
                      bidsArray    = bids (msg message)

instance (ExchangeOrder a) => ExchangeOrder (Maybe a) where
  toOrder (Just message) = toOrder message
  toOrder Nothing        = []



getCurrencyPairFromMessage :: BitstampMessage -> CurrencyPair
getCurrencyPairFromMessage bitstamp = getCurrencyPairFromChannel $ channel bitstamp

getCurrencyPairFromChannel :: String -> CurrencyPair
getCurrencyPairFromChannel "diff_order_book_ltcusd" = LTCUSD
getCurrencyPairFromChannel "diff_order_book_bchusd" = BCHUSD
getCurrencyPairFromChannel "diff_order_book_ethusd" = ETHUSD
getCurrencyPairFromChannel "diff_order_book_xrpusd" = XRPUSD
