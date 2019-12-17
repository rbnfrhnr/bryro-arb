{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Exchange.Binance.Decoder (
       BinanceMessage(..)
) where

import Data.Aeson
import GHC.Generics
import Exchange.Types
import Finance.Types


{- | represents a WebSocket Message for an orderbook Update -}
data BinanceMessage = BinanceMessage {
    event     :: !String     -- ^ name of the event. defined by binance to identify what kind of message was received
   ,eventTime :: !Int        -- ^ timestamp of the message
   ,symbol    :: !String     -- ^ currencypair for which the update was received
   ,bids      :: ![[Double]] -- ^ array which represents a tuple. fst: price, snd: qty
   ,asks      :: ![[Double]] -- ^ array which represents a tuple. fst: price, snd: qty
} deriving (Show, Generic)


instance FromJSON BinanceMessage where
             parseJSON (Object v) = do
                                        event <- v .:? "e" .!= ""
                                        asks <- v .:? "a" .!= []
                                        bids <- v .:? "b" .!= []
                                        timestamp <- v .:? "E" .!= 0
                                        symbol <- v .:? "s" .!= ""
                                        return (BinanceMessage {  asks = map (map read) asks :: [[Double]],
                                                        bids = map (map read ) bids :: [[Double]],
                                                        eventTime = timestamp * 1000,
                                                        symbol = symbol,
                                                        event = event})
instance ToJSON BinanceMessage


instance ExchangeOrder BinanceMessage
    where toOrder message = (map (\(price:qty:[]) -> AskOrder (BaseOrder Binance currencyPair price qty timestamp)) asksArray) ++ (map (\(price:qty:[]) -> BidOrder (BaseOrder Binance currencyPair price qty timestamp)) bidsArray)
                    where timestamp    = eventTime message
                          currencyPair = toCurrencyPair $ symbol message
                          asksArray    = asks message
                          bidsArray    = bids message

instance (ExchangeOrder a) => ExchangeOrder (Maybe a)
  where toOrder (Just message) = toOrder message
        toOrder Nothing        = []

{- | maps the currencypair used by binance to our internal representation-}
toCurrencyPair :: String -> CurrencyPair
toCurrencyPair "LTCUSDT" = LTCUSD
toCurrencyPair "XRPUSDT" = XRPUSD
toCurrencyPair "ETHUSDT" = ETHUSD
toCurrencyPair "BCHUSDT" = BCHUSD
