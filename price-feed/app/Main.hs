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

       let topic = "price-feed"
           run = runKafka $ mkKafkaState "price-feed-client" ("localhost", 9092)
--           byteMessages = fmap (TopicAndMessage topic . makeMessage)
--             requireAllAcks = do
--               stateRequiredAcks .= -1
--               stateWaitSize .= 1
--               stateWaitTime .= 1000

       let worker queue = do
                          orders <- Chan.readChan queue
                          let byteStringOrder =  BL.toStrict $ Aeson.encode orders
--                          putStrLn $ show $ Aeson.encode orders
                          putStrLn $ show byteStringOrder
                          result <- run . produceMessages $ [TopicAndMessage topic $ makeMessage byteStringOrder]
                          putStrLn $ show result
                          worker queue
       worker orderQueue


