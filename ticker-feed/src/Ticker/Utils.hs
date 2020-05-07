{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Ticker.Utils
  ( CurrencyExchangeKey
  , DBookMap
  , Destination(..)
  , TickBuffer
  , TickerST(..)
  , WriteOutIO
  , SimpleOut(..)
  , printTickFiltered
  , toCurrencyExchangeKey
  , writeOutIO
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map                as Map

import           Control.Monad
import           Data.Aeson
import           Data.Time               (UTCTime)
import           Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import           Database.InfluxDB.Line
import           Finance.OrderBook.Types
import           Finance.Types
import           Finance.Utils
import           Network.Kafka
import           Network.Kafka.Producer
import           System.IO
import           Utils.Influx            as Influx
import           Utils.Kafka             as Kafka

instance KafkaData Tick where
  toKafkaData tick = makeMessage $ BL.toStrict $ encode tick

instance WriteOutIO Kafka.WriteKafka where
  writeOutIO wKafka tick = Kafka.writeToKafka (\msg -> return ()) wKafka tick >> return wKafka :: IO Kafka.WriteKafka

--instance WriteOutIO Influx.InfluxConnection where
--  writeOutIO influxCon tick = Influx.writeToInflux influxCon tick
instance WriteOutIO SimpleOut where
  writeOutIO simple tick = printTickFiltered (Just "LTCUSDBitstamp") tick >> hFlush stdout >> return SimpleOut

instance WriteOutIO Destination where
  writeOutIO (Destination desti) tick = Destination <$> writeOutIO desti tick

instance WriteOutIO [Destination] where
  writeOutIO destinations tick =
    foldM (\list (Destination dest) -> fmap (: list) (Destination <$> writeOutIO dest tick)) [] destinations

type CurrencyExchangeKey = String

type DBookMap = Map.Map CurrencyExchangeKey OrderBook

type TickBuffer = Map.Map CurrencyExchangeKey Tick

data SimpleOut =
  SimpleOut

data Destination =
  forall a. WriteOutIO a =>
            Destination a

data TickerST =
  TickerST
    { tickerDBookMap     :: !DBookMap
    , tickerBuffer       :: !TickBuffer
    , tickerDestinations :: ![Destination]
    , tickerOrderQueue   :: C.Chan [BaseOrder]
    }

toCurrencyExchangeKey :: BaseOrder -> String
toCurrencyExchangeKey order = show (orderCurrencyPair order) ++ show (orderExchange order)

printTickFiltered :: Maybe CurrencyExchangeKey -> Tick -> IO ()
printTickFiltered Nothing tick = printTick tick
printTickFiltered (Just currExchangeKey) tick@(Tick (Just ask) (Just bid) (Just currency) (Just exchange) timestamp)
  | currExchangeKey == show currency ++ show exchange = printTick tick
  | otherwise = return ()
printTickFiltered _ _ = return ()

printTick :: Tick -> IO ()
printTick (Tick (Just ask) (Just bid) (Just currency) (Just exchange) timestamp) =
  putStrLn (show exchange ++ " -> " ++ show currency ++ " - " ++ show ask ++ " / " ++ show bid) >> hFlush stdout
printTick _ = return ()

class WriteOutIO a where
  writeOutIO :: a -> Tick -> IO a
