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
       [ Destination (PrintTick (Just "LTCUSDBinance"))
       , Destination (writeHandle kafkaConfig "bryro-ticker" 0)
       , Destination influxHandle
       ]
       orderQueue)

runTransform :: TickerHandle -> IO ()
runTransform tickerST@(TickerHandle dBookMap tBuffer _ queue) =
  Chan.readChan queue >>= return . foldl handleDepthBook tickerST >>=
  (\tickerST ->
     foldM
       (\tst key -> bufferedWrite tst key (Map.lookup key (tickerBuffer tst)) (Map.lookup key (tickerDBookMap tickerST)))
       tickerST
       (Map.keys (tickerDBookMap tickerST))) >>=
  runTransform >>
  return ()

handleDepthBook :: TickerHandle -> BaseOrder -> TickerHandle
handleDepthBook tickerST@(TickerHandle dBookMap _ _ _) order =
  case Map.lookup dBookMapKey dBookMap of
    Just depthBook -> tickerST {tickerDBookMap = Map.insert dBookMapKey (updateDepthBook depthBook order) dBookMap}
    Nothing -> handleDepthBook tickerST {tickerDBookMap = Map.insert dBookMapKey newOrderBook dBookMap} order
  where
    dBookMapKey = toCurrencyExchangeKey order
    newOrderBook = openOrderBook (orderExchange order) (orderCurrencyPair order)

bufferedWrite :: TickerHandle -> CurrencyExchangeKey -> Maybe Tick -> Maybe OrderBook -> IO TickerHandle
bufferedWrite tickerST@(TickerHandle dbookMap tickBuffer dest queue) key (Just currentTick) (Just book)
  | currentTick /= latestTick =
    writeOutIO dest latestTick >>= (\uDest -> return (TickerHandle dbookMap updatedTickBuffer uDest queue))
  | otherwise = return tickerST
  where
    latestTick = getTick book
    updatedTickBuffer = Map.insert key latestTick tickBuffer
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
