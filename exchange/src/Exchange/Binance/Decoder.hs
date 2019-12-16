{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Exchange.Binance.Decoder (
       BinanceMessage(..)
) where

import Data.Aeson
import GHC.Generics
import Exchange.Types
import Finance.Types


data BinanceMessage = BinanceMessage {
    event :: String,
    eventTime :: Int,
    symbol :: String,
    bids :: [[Double]],
    asks :: [[Double]]
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

toCurrencyPair :: String -> CurrencyPair
toCurrencyPair "LTCUSDT" = LTCUSD
toCurrencyPair "XRPUSDT" = XRPUSD
toCurrencyPair "ETHUSDT" = ETHUSD
toCurrencyPair "BCHUSDT" = BCHUSD
