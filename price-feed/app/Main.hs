{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as Map
import qualified Database.InfluxDB.Format as F

import           Prelude                  hiding (lookup)

import           Control.Concurrent
import           Control.Concurrent.Chan  as Chan
import           Control.Exception
import           Control.Monad
import           Data.ByteString
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Text
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
import           Network.Kafka.Protocol
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Utils.Influx             as Influx
import           Utils.Kafka              as Kafka

configFile :: IO (Either SomeException Config)
configFile = try $ load [Required $ "resources" </> "config.cfg"]

main :: IO ()
main = do
  mb <- configFile
  case mb of
    Left (err :: SomeException) -> print err
    Right cfg                   -> runFeed cfg

runFeed :: Config -> IO ()
runFeed cfg = do
  Prelude.putStrLn "Started order-feed"
  orderQueue <- newChan
  Bitstamp.subscribeHandler $ decodeAndEnQueueHandler Bitstamp.parseToOrder orderQueue
  Kraken.subscribeHandler $ decodeAndEnQueueHandler Kraken.parseToOrder orderQueue
  Binance.subscribeHandler $ decodeAndEnQueueHandler Binance.parseToOrder orderQueue
  writeKafkaHandle <- fmap (\kafkaCfg -> writeHandle kafkaCfg "bryro-orders" 1) (Kafka.createKafkaConfig cfg)
  influxHandle <- Influx.new cfg
  let worker queue influxConn writeKafkaHandler = do
        orders <- Chan.readChan queue
                                      {- todo can most definitely be made into one fold -}
        writeKafkaST2 <- writeToKafka writeKafkaHandler orders
        influxHandle' <- foldM writeAsync influxConn orders
        worker queue influxHandle' writeKafkaST2
  worker orderQueue influxHandle writeKafkaHandle

instance InfluxData BaseOrder where
  toInfluxData (BaseOrder exchange currency price quantity time orderType) =
    (tags, fields, Just utcTime :: Maybe UTCTime)
    where
      tags =
        Map.fromList
          [ ("currency", stringFormatter (show currency))
          , ("exchange", stringFormatter (show exchange))
          , ("type", stringFormatter (show orderType))
          ]
      fields = Map.fromList [("price", FieldFloat price), ("quantity", FieldFloat quantity)]
      utcTime = posixSecondsToUTCTime $ realToFrac $ fromIntegral time / (1000 * 1000)
      stringFormatter = formatKey F.string

instance KafkaData [BaseOrder] where
  toKafkaData order = makeMessage $ BL.toStrict $ Aeson.encode order
