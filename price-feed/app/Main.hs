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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Time
import Data.Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Producer
import Network.Kafka.Protocol
import Database.InfluxDB
import qualified Database.InfluxDB.Format as F
import qualified Text.Show.ByteString as SB
import Control.Lens
import Finance.Types
import Data.Configurator
import Data.Configurator.Types
import System.FilePath
import System.IO
import System.Directory
import System.Environment
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Int
import Influx
import Kafka

data PriceFeedConfig = PriceFeedConfig KafkaConfig InfluxConfig

createConfig :: Config -> IO PriceFeedConfig
createConfig config = do
                      kafkaConfig <- createKafkaConfig config
                      influxConfig <- createInfluxConfig config
                      return (PriceFeedConfig kafkaConfig influxConfig)

configFile :: FilePath -> [Worth FilePath]
configFile name = [Required $ "resources" </> name]

main :: IO ()
main = do
       mb <- try $ load (configFile "config.cfg")
       case mb of
           Left (err :: SomeException) -> Prelude.putStrLn $ show err
           Right cfg -> createConfig cfg >>= runFeed


runFeed :: PriceFeedConfig -> IO ()
runFeed (PriceFeedConfig kafkaConf influxConf) = do
                                                 Prelude.putStrLn "Started order-feed"
                                                 orderQueue <- newChan
                                                 Bitstamp.subscribeToDepthBook $ orderFeedHandler orderQueue Bitstamp.parseBitstampMessage
                                                 Binance.subscribeToDepthBook $ orderFeedHandler orderQueue Binance.parseBinanceMessage
                                                 Kraken.subscribeToDepthBook $ orderFeedHandler orderQueue Kraken.parseKrakenMessage

                                                 let run = runKafka $ mkKafkaState "price-feed-client" (kafkaHost', kafkaPort')
                                                 let byteMessages = fmap (TopicAndMessage topic . makeMessage . BL.toStrict)
                                                 influxConn <- getConnectionConf influxConf

                                                 let worker queue influxConn = do
                                                                         orders <- Chan.readChan queue
                                                                         result <- run . produceMessages $ byteMessages [(Aeson.encode orders)]
                                                                         influxConn2 <- writeToInflux influxConn (Prelude.map (mapToInfluxData) orders)
                                                                         worker queue influxConn2
                                                 worker orderQueue influxConn


                                                 where topic = TName $ KString $ kafkaTopic kafkaConf
                                                       kafkaHost' = Host $ KString $ kafkaHost kafkaConf
                                                       kafkaPort' = Port ((fromIntegral (kafkaPort kafkaConf)) :: Int32)

mapToInfluxData :: Order -> Line UTCTime
mapToInfluxData (AskOrder (BaseOrder exchange currency price quantity time)) = constructLine "Ask" exchange currency price quantity time
mapToInfluxData (BidOrder (BaseOrder exchange currency price quantity time)) = constructLine "Bid" exchange currency price quantity time

constructLine :: String -> Exchange -> CurrencyPair -> Double -> Double -> Int -> Line UTCTime
constructLine orderType exchange currency price quantity time = Line "order" tags fields (Just utcTime :: Maybe UTCTime)
                                                              where tags            = (Map.fromList [("currency" ,  (stringFormatter (show currency))), ("exchange", (stringFormatter (show exchange))), ("type",  (stringFormatter orderType))])
                                                                    fields          = (Map.fromList [("price", FieldFloat price), ("quantity", FieldFloat quantity)])
                                                                    utcTime         = posixSecondsToUTCTime $ realToFrac $ (fromIntegral time) / (1000 * 1000)
                                                                    stringFormatter = formatKey F.string
