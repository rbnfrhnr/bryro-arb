{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import qualified Control.Concurrent.Chan  as Chan

import           Control.Exception
import           Control.Monad
import           Data.Configurator
import           Data.Configurator.Types
import           Exchange.Binance.Utils   as Binance
import           Exchange.Bitstamp.Utils  as Bitstamp
import           Exchange.Handler
import           Exchange.Kraken.Utils    as Kraken
import           Finance
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
