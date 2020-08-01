{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import qualified Control.Concurrent.Chan  as Chan
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as Map
import qualified Database.InfluxDB.Format as F

import           Control.Exception
import           Control.Monad
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Database.InfluxDB
import           Exchange.Binance.Utils   as Binance
import           Exchange.Bitstamp.Utils  as Bitstamp
import           Exchange.Handler
import           Exchange.Kraken.Utils    as Kraken
import           Finance
import           Network.Kafka
import           Network.Kafka.Producer
import           Order.Utils
import           System.FilePath
import           System.IO
import           Utils.Forward            (Destination (..), SimpleOut (..),
                                           WriteOutIO, writeOutIO)
import           Utils.Influx             as Influx
import           Utils.Kafka              as Kafka

main :: IO ()
main = configFile >>= either print (Main.init >=> run)
  where
    configFile = try $ load [Required $ "resources" </> "config.cfg"] :: IO (Either SomeException Config)

init :: Config -> IO OrderFeedHandle
init cfg = do
  orderQueue <- Chan.newChan
  Bitstamp.subscribeHandler $ decodeAndEnQueueHandler Bitstamp.parseToOrder orderQueue
  Kraken.subscribeHandler $ decodeAndEnQueueHandler Kraken.parseToOrder orderQueue
  Binance.subscribeHandler $ decodeAndEnQueueHandler Binance.parseToOrder orderQueue
  writeKafkaHandle <- fmap (\kafkaCfg -> writeHandle kafkaCfg "bryro-orders" 1) (Kafka.createKafkaConfig cfg)
  influxHandle <- Influx.new cfg
  return (OrderFeedHandle orderQueue [Destination writeKafkaHandle, Destination influxHandle])

run :: OrderFeedHandle -> IO ()
run (OrderFeedHandle queue destinations) =
  Chan.readChan queue >>= writeOutIO destinations >>= run . newOrderFeedHandle >> return ()
  where
    newOrderFeedHandle destinations' = OrderFeedHandle queue destinations'
