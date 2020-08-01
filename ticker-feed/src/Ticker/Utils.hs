{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Ticker.Utils
  ( CurrencyExchangeKey
  , DBookMap
  , Destination(..)
  , PrintTick(..)
  , TickBuffer
  , TickerHandle(..)
  , WriteOutIO
  , printTickFiltered
  , toCurrencyExchangeKey
  , writeOutIO
  ) where

import qualified Control.Concurrent.Chan  as C
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as Map
import qualified Database.InfluxDB.Format as F

import           Control.Monad
import           Data.Aeson
import           Data.Time                (UTCTime)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           Database.InfluxDB        (formatKey)
import           Database.InfluxDB.Line
import           Finance
import           Network.Kafka
import           Network.Kafka.Producer
import           System.IO
import           Utils.Forward
import           Utils.Influx             as Influx
import           Utils.Kafka              as Kafka

type CurrencyExchangeKey = String

type DBookMap = Map.Map CurrencyExchangeKey OrderBook

type TickBuffer = Map.Map OrderBookKey Tick

data PrintTick =
  PrintTick (Maybe Exchange) (Maybe CurrencyPair)

data TickerHandle =
  TickerHandle
    { tickerDBookGroup   :: !OrderBookGroup
    , tickerBuffer       :: !TickBuffer
    , tickerDestinations :: ![Destination Tick]
    , tickerOrderQueue   :: C.Chan [BaseOrder]
    }

instance InfluxData Tick where
  toInfluxData (Tick (askPrice, askQty) (bidPrice, bidQty) currency exchange timestamp) =
    (tags, fields, Just utcTime :: Maybe UTCTime)
    where
      tags = Map.fromList [("currency", stringFormatter (show currency)), ("exchange", stringFormatter (show exchange))]
      fields =
        Map.fromList
          [ ("askPrice", FieldFloat askPrice)
          , ("askQty", FieldFloat askQty)
          , ("bidPrice", FieldFloat bidPrice)
          , ("bidQty", FieldFloat bidQty)
          ]
      utcTime = posixSecondsToUTCTime $ realToFrac $ fromIntegral timestamp / (1000 * 1000)
      stringFormatter = formatKey F.string

instance KafkaData Tick where
  toKafkaData tick = makeMessage $ BL.toStrict $ encode tick

{- | instance for writing a Tick to Kafka. Can now be used with (Destination WriteHandle) -}
instance WriteOutIO Kafka.WriteHandle Tick where
  writeOutIO wKafka tick = Kafka.writeToKafka wKafka tick >> return wKafka :: IO Kafka.WriteHandle

{- | instance for writing a Tick to Influx. Can now be used with (Destination InfluxHandle) -}
instance WriteOutIO Influx.InfluxHandle Tick where
  writeOutIO influxCon tick = Influx.writeAsync influxCon tick

{- | Overwriting default print, since Tick is an instance of Show -}
instance WriteOutIO PrintTick Tick where
  writeOutIO printTick tick = printTickFiltered printTick tick >> hFlush stdout >> return printTick

toCurrencyExchangeKey :: BaseOrder -> String
toCurrencyExchangeKey order = show (orderCurrencyPair order) ++ show (orderExchange order)

printTickFiltered :: PrintTick -> Tick -> IO ()
printTickFiltered (PrintTick (Just exchange) (Just currency)) tick@(Tick _ _ tickCurrency tickExchange _)
  | exchange == tickExchange && currency == tickCurrency = printTick tick
  | otherwise = return ()
printTickFiltered (PrintTick (Just exchange) Nothing) tick@(Tick _ _ _ tickExchange _)
  | exchange == tickExchange = printTick tick
  | otherwise = return ()
printTickFiltered (PrintTick Nothing (Just currency)) tick@(Tick _ _ tickCurrency _ _)
  | currency == tickCurrency = printTick tick
  | otherwise = return ()
printTickFiltered _ tick = printTick tick

printTick :: Tick -> IO ()
printTick (Tick ask bid currency exchange timestamp) =
  putStrLn (show exchange ++ " -> " ++ show currency ++ " - " ++ show ask ++ " / " ++ show bid) >> hFlush stdout
