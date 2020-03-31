{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)

import qualified Data.Aeson as Aeson
import Exchange.Bitstamp.Utils as Bitstamp
import Exchange.Binance.Utils as Binance
import Exchange.Kraken.Utils as Kraken
import Exchange.Network.Utils
import Control.Concurrent
import Control.Concurrent.Chan as Chan
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Time
import Data.Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.Kafka.Producer
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
import Influx as Influx
import Kafka as Kafka


configFile :: FilePath -> [Worth FilePath]
configFile name = [Required $ "resources" </> name]

main :: IO ()
main = do
       mb <- try $ load (configFile "config.cfg")
       case mb of
           Left (err :: SomeException) -> Prelude.putStrLn $ show err
           Right cfg -> runFeed cfg


runFeed :: Config -> IO ()
runFeed cfg = do
              Prelude.putStrLn "Started order-feed"
              orderQueue <- newChan

              Bitstamp.subscribeToDepthBook $ orderFeedHandler orderQueue Bitstamp.parseBitstampMessage
              Binance.subscribeToDepthBook $ orderFeedHandler orderQueue Binance.parseBinanceMessage
              Kraken.subscribeToDepthBook $ orderFeedHandler orderQueue Kraken.parseKrakenMessage

              kafkaConn <- Kafka.getConnection cfg
              influxConn <- Influx.getConnection cfg

              let worker queue influxConn kafkaConn = do
                                      orders <- Chan.readChan queue
                                      {- todo can most definitely be made into one fold -}
                                      influxConn2 <- foldM (writeToInflux) influxConn orders
                                      kafkaConn2 <- foldM writeToKafka kafkaConn orders
                                      worker queue influxConn2 kafkaConn2
              worker orderQueue influxConn kafkaConn

instance InfluxData Order where
    toInfluxLine (AskOrder (BaseOrder exchange currency price quantity time)) = constructLine "Ask" exchange currency price quantity time
    toInfluxLine (BidOrder (BaseOrder exchange currency price quantity time)) = constructLine "Bid" exchange currency price quantity time

constructLine :: String -> Exchange -> CurrencyPair -> Double -> Double -> Int -> Line UTCTime
constructLine orderType exchange currency price quantity time = Line "order" tags fields (Just utcTime :: Maybe UTCTime)
                                                              where tags            = (Map.fromList [("currency" ,  (stringFormatter (show currency))), ("exchange", (stringFormatter (show exchange))), ("type",  (stringFormatter orderType))])
                                                                    fields          = (Map.fromList [("price", FieldFloat price), ("quantity", FieldFloat quantity)])
                                                                    utcTime         = posixSecondsToUTCTime $ realToFrac $ (fromIntegral time) / (1000 * 1000)
                                                                    stringFormatter = formatKey F.string

instance KafkaData Order where
    toKafkaData order =  makeMessage $ BL.toStrict $ Aeson.encode order
