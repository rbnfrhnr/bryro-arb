{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson as Aeson
import Exchange.Bitstamp.Utils as Bitstamp
import Exchange.Binance.Utils as Binance
import Exchange.Kraken.Utils as Kraken
import Control.Concurrent
import Control.Concurrent.Chan as Chan
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Producer
import Network.Kafka.Protocol (ProduceResponse(..),
                               KafkaError(..),
                               CompressionCodec(..),
                               CreateTopicsResponse(..),
                               DeleteTopicsResponse(..),
                               Offset(..),
                               OffsetCommitResponse(..),
                               OffsetFetchResponse(..),
                               ConsumerGroup(..),
                               Partition(..),
                               KafkaString(..),
                               Metadata(..),
                               TopicName(..)
                              )



main :: IO ()
main = do
       orderQueue <- newChan
       Bitstamp.subscribeToDepthBook orderQueue
       Binance.subscribeToDepthBook orderQueue
       Kraken.subscribeToDepthBook orderQueue

       let topic = "bryro-orders"
--           run = runKafka $ mkKafkaState "price-feed-client" ("217.162.130.76", 9092)
--           run = runKafka $ mkKafkaState "price-feed-client" ("217.162.130.76", 9091)
           run = runKafka $ mkKafkaState "price-feed-client" ("localhost", 9092)
           byteMessages = fmap (TopicAndMessage topic . makeMessage . BL.toStrict)

       let worker queue = do
                          orders <- Chan.readChan queue
                          let byteStringOrder =  BL.toStrict $ Aeson.encode orders
                          putStrLn $ show byteStringOrder
                          result <- run . produceMessages $ byteMessages [(Aeson.encode orders)]
                          putStrLn $ show result
                          worker queue
       worker orderQueue


