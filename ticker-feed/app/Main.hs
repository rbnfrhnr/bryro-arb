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
import           Finance.Depthbook.Types
import           Finance.Depthbook.Utils
import           Finance.Types
import           Network.Kafka
import           Network.Kafka.Producer
import           System.FilePath
import           System.IO
import           Ticker.Utils
import           Utils.Influx            as Influx
import           Utils.Kafka             as Kafka

configFileKafka :: IO (Either SomeException Config)
configFileKafka = try $ load [Required $ "ticker-feed" </> "resources" </> "config.cfg"]

main :: IO ()
main = configFileKafka >>= withConfig

withConfig :: Either SomeException Config -> IO ()
withConfig (Right cfg) = do
  kafkaState <- fmap Kafka.configToKafkaState (Kafka.createKafkaConfig cfg)
  orderQueue <- Chan.newChan
  Bitstamp.subscribeHandler $ decodeAndEnQueueHandler Bitstamp.parseToOrder orderQueue
  Kraken.subscribeHandler $ decodeAndEnQueueHandler Kraken.parseToOrder orderQueue
  Binance.subscribeHandler $ decodeAndEnQueueHandler Binance.parseToOrder orderQueue
  runTransform orderQueue (writeKafkaState kafkaState "bryro-ticker" 0) Map.empty Map.empty

runTransform :: Chan.Chan [Order] -> WriteKafka -> TickBuffer -> DBookMap -> IO ()
runTransform queue wKafka tickBuffer dBookMap =
  Chan.readChan queue >>= return . foldl handleDepthBook dBookMap >>=
  (\bookMap ->
     foldM
       (\(_, tBuffer) key ->
          bufferedWrite wKafka (Map.lookup key tickBuffer) (Map.lookup key dBookMap) >>=
          (\nTick -> return (bookMap, Map.insert key nTick tBuffer)))
       (bookMap, tickBuffer)
       (Map.keys bookMap)) >>=
  (\(bookM, tiBuffer) -> runTransform queue wKafka tiBuffer bookM) >>
  return ()

handleDepthBook :: DBookMap -> Order -> DBookMap
handleDepthBook dBookMap order =
  case Map.lookup currencyExchangeKey dBookMap of
    Just depthBook -> Map.insert currencyExchangeKey (updateDepthBook depthBook order) dBookMap
    Nothing -> handleDepthBook (Map.insert currencyExchangeKey (openDepthBook (getCurrencyPair order)) dBookMap) order
  where
    currencyExchangeKey = toCurrencyExchangeKey order

bufferedWrite :: WriteKafka -> Maybe (Maybe Order, Maybe Order) -> Maybe DepthBook -> IO (Maybe Order, Maybe Order)
bufferedWrite wKafka (Just (lastAsk, lastBid)) (Just book)
  | (lastAsk, lastBid) /= tickPair = Kafka.writeToKafka (\_ -> return ()) wKafka tickPair >> return tickPair
  | otherwise = return (lastAsk, lastBid)
  where
    minAsk = fmap snd (Map.lookupMin (depthBookAsk book))
    maxBid = fmap snd (Map.lookupMax (depthBookBid book))
    tickPair = (minAsk, maxBid)
bufferedWrite wKafka Nothing (Just book) = Kafka.writeToKafka (\msg -> return ()) wKafka tickPair >> return tickPair
  where
    minAsk = fmap snd (Map.lookupMin (depthBookAsk book))
    maxBid = fmap snd (Map.lookupMax (depthBookBid book))
    tickPair = (minAsk, maxBid)
bufferedWrite wKafka lastTick book = return (Nothing, Nothing)

decodeOrders :: Either KafkaClientError [BS.ByteString] -> IO [Order]
decodeOrders (Right msg) = return (foldl filteredDecode [] msg) :: IO [Order]
decodeOrders (Left err)  = fail "vla"

filteredDecode :: [Order] -> BS.ByteString -> [Order]
filteredDecode orders bsMessage =
  case decode (BL.fromStrict bsMessage) :: Maybe [Order] of
    (Just order) -> order ++ orders
    Nothing      -> orders
