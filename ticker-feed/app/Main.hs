{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map                as Map

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Configurator
import           Data.Configurator.Types
import           Exchange.Binance.Utils  as Binance
import           Exchange.Bitstamp.Utils as Bitstamp
import           Exchange.Handler
import           Exchange.Kraken.Utils   as Kraken
import           Finance
import           Network.Kafka
import           Network.Kafka.Producer
import           System.FilePath
import           System.IO
import           Ticker.Utils
import           Utils.Influx            as Influx
import           Utils.Kafka             as Kafka

configFileKafka :: IO (Either SomeException Config)
configFileKafka = try $ load [Required $ "resources" </> "config.cfg"]

main :: IO ()
main = configFileKafka >>= withConfig

withConfig :: Either SomeException Config -> IO ()
withConfig (Right cfg) = do
  kafkaConfig <- Kafka.createKafkaConfig cfg
  orderQueue <- Chan.newChan
  Bitstamp.subscribeHandler $ decodeAndEnQueueHandler Bitstamp.parseToOrder orderQueue
  Kraken.subscribeHandler $ decodeAndEnQueueHandler Kraken.parseToOrder orderQueue
  Binance.subscribeHandler $ decodeAndEnQueueHandler Binance.parseToOrder orderQueue
  influxHandle <- Influx.new cfg :: IO Influx.InfluxHandle
  runTransform
    (TickerHandle
       Map.empty
       Map.empty
       [Destination (writeHandle kafkaConfig "bryro-ticker" 0), Destination influxHandle]
       orderQueue)

{- | Read from the order queue, update the order books and finally extract the ticks and send them to influx/kafka/stdout if necessary
-}
runTransform :: TickerHandle -> IO ()
runTransform tickerST@(TickerHandle dBookMap tBuffer _ queue) =
  Chan.readChan queue >>= return . foldl handleOrderBook tickerST >>=
  (\tickerST ->
     foldM
       (\tst key -> bufferedWrite tst key (Map.lookup key (tickerBuffer tst)) (Map.lookup key (tickerDBookMap tickerST)))
       tickerST
       (Map.keys (tickerDBookMap tickerST))) >>=
  runTransform >>
  return ()

{- | If there already is an orderBook for the currency and exchange referred by to by the order,
     it will update the order book. If there there isn't an orderBook for the currency/exchange,
     it will create a new book, insert the order and add it to the handle
-}
handleOrderBook :: TickerHandle -> BaseOrder -> TickerHandle
handleOrderBook tickerST@(TickerHandle dBookMap _ _ _) order =
  case Map.lookup dBookMapKey dBookMap of
    Just orderBook -> tickerST {tickerDBookMap = Map.insert dBookMapKey (updateOrderBook orderBook order) dBookMap}
    Nothing -> handleOrderBook tickerST {tickerDBookMap = Map.insert dBookMapKey newOrderBook dBookMap} order
  where
    dBookMapKey = toCurrencyExchangeKey order
    newOrderBook = openOrderBook (orderExchange order) (orderCurrencyPair order)

{- | Checks whether the tick extracted from the orderBook differs from the last one that got sent.
     In case they differ, the new tick will be written to kafka, influx etc.

     First match handles the case when there already is a tick present in the buffer.
     Second match handles the case when there is not tick present in the buffer
     (e.g first time extracting tick from orderBook)
-}
bufferedWrite :: TickerHandle -> CurrencyExchangeKey -> Maybe Tick -> Maybe OrderBook -> IO TickerHandle
bufferedWrite tickerST@(TickerHandle dbookMap tickBuffer dest queue) key (Just currentTick) (Just book)
  | currentTick /= latestTick = fmap tickerHandle' (writeOutIO dest latestTick)
  | otherwise = return tickerST
  where
    latestTick = getTick book
    updatedTickBuffer = Map.insert key latestTick tickBuffer
    tickerHandle' dest = TickerHandle dbookMap updatedTickBuffer dest queue
bufferedWrite tickerST@(TickerHandle dbookMap tickBuffer dest queue) key Nothing (Just book) =
  writeOutIO dest tickPair >>= (\uDest -> return (TickerHandle dbookMap updatedTickBuffer uDest queue))
  where
    tickPair = getTick book
    updatedTickBuffer = Map.insert key tickPair tickBuffer
bufferedWrite tickerST key lastTick book = return tickerST

decodeOrders :: Either KafkaClientError [BS.ByteString] -> IO [BaseOrder]
decodeOrders (Right msg) = return (foldl filteredDecode [] msg) :: IO [BaseOrder]
decodeOrders (Left err) = fail (show err)

filteredDecode :: [BaseOrder] -> BS.ByteString -> [BaseOrder]
filteredDecode orders bsMessage =
  case decode (BL.fromStrict bsMessage) :: Maybe [BaseOrder] of
    (Just order) -> order ++ orders
    Nothing      -> orders
