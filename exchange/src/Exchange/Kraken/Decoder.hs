{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Exchange.Kraken.Decoder(
       KrakenMessage(..)
) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson.Types
import Finance.Types
import Exchange.Types

data KrakenMessage = KrakenMessage{
    channelName :: String,
    symbol :: String,
    asks :: [[Double]],
    bids :: [[Double]],
    timestamp :: Int
} deriving (Show, Generic)

instance FromJSON KrakenMessage where
        parseJSON (Array a) = do
                            let list = V.toList a
                            kraken <- getKrakenMessage list

                            return $ kraken
        parseJSON _ = return $ KrakenMessage "" "" [] [] 1

getKrakenMessage :: [Value] -> Parser KrakenMessage
getKrakenMessage (id:asks:bids:channel:symbol:[]) = do
                                            channelString <- parseJSON channel
                                            symbolString <- parseJSON symbol
                                            pricesObject <- parseJSON asks
                                            pricesObjectBid <- parseJSON bids
                                            askPrices <- getPrices pricesObject "a"
                                            bidPrices <- getPrices pricesObjectBid "b"
                                            timestamp <- getTimestamp pricesObject "a"
                                            timestamp2 <- getTimestamp pricesObject "b"
                                            return $ KrakenMessage channelString symbolString askPrices bidPrices (getJustTimestamp timestamp timestamp2)
getKrakenMessage (id:asksOrBids:channel:symbol:[]) = do
                                            channelString <- parseJSON channel
                                            symbolString <- parseJSON symbol
                                            pricesObject <- parseJSON asksOrBids
                                            askPrices <- getPrices pricesObject "a"
                                            bidPrices <- getPrices pricesObject "b"
                                            timestamp <- getTimestamp pricesObject "a"
                                            timestamp2 <- getTimestamp pricesObject "b"
                                            return $ KrakenMessage channelString symbolString askPrices bidPrices (getJustTimestamp timestamp timestamp2)


getPrices :: Object -> String -> Parser [[Double]]
getPrices prices jsonKey = do
                            asks <- prices .:? (T.pack jsonKey) .!= [] :: Parser [[String]]
                            initialPrices <- prices .:? (T.pack (jsonKey ++ "s")) .!= [] :: Parser [[String]]
                            let finalPrices =  map ( map read ) (patchPricesArray asks) :: [[Double]]
                            let finalInitialPrices =  map ( map read ) (patchPricesArray initialPrices) :: [[Double]]
                            return (finalPrices ++ finalInitialPrices)

{- we get seconds but want micros-}
getJustTimestamp :: Int -> Int -> Int
getJustTimestamp 0 timestamp2 = timestamp2 * 1000000
getJustTimestamp timestamp1 0 = timestamp1 * 1000000
getJustTimestamp _ _ = 0


getTimestamp :: Object -> String -> Parser Int
getTimestamp prices jsonKey = do
                                pricesParser <- prices .:? (T.pack jsonKey) .!= [] :: Parser [[String]]
                                initialMessageParser <- prices .:? (T.pack (jsonKey ++ "s")) .!= [] :: Parser [[String]]

                                let updateTimestamp = getTimestampFromArray pricesParser
                                let initialMessageTimestamp = getTimestampFromArray initialMessageParser
                                return (initialMessageTimestamp + updateTimestamp)

patchPricesArray :: [[String]] -> [[String]]
patchPricesArray prices = map (Prelude.take 2) prices

getTimestampFromArray :: [[String]] -> Int
getTimestampFromArray [] = 0
getTimestampFromArray ([_, _, timestampAsString]:xs) = floor (read timestampAsString :: Double)

instance ExchangeOrder KrakenMessage
  where toOrder message = (map (\(price:qty:[]) -> AskOrder (BaseOrder Kraken currencyPair price qty msgTimestamp)) asksArray) ++ (map (\(price:qty:[]) -> BidOrder (BaseOrder Kraken currencyPair price qty msgTimestamp)) bidsArray)
                where asksArray = asks message
                      bidsArray = bids message
                      msgTimestamp = timestamp message
                      currencyPair = toCurrencyPair $ symbol message

instance (ExchangeOrder a) => ExchangeOrder (Maybe a)
  where toOrder (Just message) = toOrder message
        toOrder Nothing        = []



toCurrencyPair :: String -> CurrencyPair
toCurrencyPair "LTC/USD" = LTCUSD
toCurrencyPair "XRP/USD" = XRPUSD
toCurrencyPair "ETH/USD" = ETHUSD
toCurrencyPair "BCH/USD" = BCHUSD
