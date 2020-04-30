{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude hiding (lookup)

import qualified Data.Aeson as Aeson
import Exchange.Bitstamp.Utils as Bitstamp
import Exchange.Binance.Utils as Binance
import Exchange.Kraken.Utils as Kraken
import Exchange.Utils
import Control.Concurrent
import Control.Concurrent.Chan as Chan
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Time
import Data.Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.InfluxDB
import qualified Database.InfluxDB.Format as F
import Finance.Types
import Data.Configurator
import Data.Configurator.Types
import System.FilePath
import System.IO
import System.Directory
import System.Environment
import Control.Exception
import Control.Monad
import Utils.Kafka as Kafka
import Utils.Influx as Influx
import Network.Kafka
import Network.Kafka.Protocol
import Network.Kafka.Producer


configFile :: IO (Either SomeException Config)
configFile = try $ load [Required $ "resources" </> "config.cfg"]

main :: IO ()
main = do
       mb <- configFile
       case mb of
           Left (err :: SomeException) -> print err
           Right cfg -> runFeed cfg


runFeed :: Config -> IO ()
runFeed cfg = do
              Prelude.putStrLn "Started order-feed"
              orderQueue <- newChan

              Bitstamp.subscribeReadonly $ orderFeedHandler orderQueue Bitstamp.parseBitstampMessage
              Binance.subscribeReadonly $ orderFeedHandler orderQueue Binance.parseBinanceMessage
              Kraken.subscribeReadonly $ orderFeedHandler orderQueue Kraken.parseKrakenMessage

              writeKafkaST <- fmap (\kafkaCfg -> writeKafkaState (Kafka.configToKafkaState kafkaCfg) "bryro-orders" 1) (Kafka.createKafkaConfig cfg)
              influxConn <- Influx.getConnection cfg

              let worker queue influxConn writeKafkaST = do
                                      orders <- Chan.readChan queue
                                      {- todo can most definitely be made into one fold -}
                                      writeKafkaST2 <- writeToKafka kafkaRespHandler writeKafkaST orders
                                      influxConn2 <- foldM writeToInflux influxConn orders
                                      worker queue influxConn2 writeKafkaST2
              worker orderQueue influxConn writeKafkaST

kafkaRespHandler :: Either KafkaClientError [ProduceResponse] -> IO ()
kafkaRespHandler (Right msgs) = return ()
kafkaRespHandler (Left err) = print err



instance InfluxData Order where
    toInfluxLine (AskOrder (BaseOrder exchange currency price quantity time)) = constructLine "Ask" exchange currency price quantity time
    toInfluxLine (BidOrder (BaseOrder exchange currency price quantity time)) = constructLine "Bid" exchange currency price quantity time

constructLine :: String -> Exchange -> CurrencyPair -> Double -> Double -> Int -> Line UTCTime
constructLine orderType exchange currency price quantity time = Line "order" tags fields (Just utcTime :: Maybe UTCTime)
                                                              where tags            = (Map.fromList [("currency" ,  (stringFormatter (show currency))), ("exchange", (stringFormatter (show exchange))), ("type",  (stringFormatter orderType))])
                                                                    fields          = (Map.fromList [("price", FieldFloat price), ("quantity", FieldFloat quantity)])
                                                                    utcTime         = posixSecondsToUTCTime $ realToFrac $ (fromIntegral time) / (1000 * 1000)
                                                                    stringFormatter = formatKey F.string

instance KafkaData [Order] where
    toKafkaData order =  makeMessage $ BL.toStrict $ Aeson.encode order
