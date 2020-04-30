{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}

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
  runTransform
    (TickerST
       Map.empty
       Map.empty
       [Destination SimpleOut, Destination (writeKafkaState kafkaState "bryro-ticker" 0)]
       orderQueue)

runTransform :: TickerST -> IO ()
runTransform tickerST@(TickerST dBookMap tBuffer _ queue) =
  Chan.readChan queue >>= return . foldl handleDepthBook tickerST >>=
  (\tickerST ->
     foldM
       (\tst key -> bufferedWrite tst key (Map.lookup key (tickerBuffer tst)) (Map.lookup key (tickerDBookMap tickerST)))
       tickerST
       (Map.keys (tickerDBookMap tickerST))) >>=
  runTransform >>
  return ()

handleDepthBook :: TickerST -> Order -> TickerST
handleDepthBook tickerST@(TickerST dBookMap _ _ _) order =
  case Map.lookup dBookMapKey dBookMap of
    Just depthBook -> tickerST {tickerDBookMap = Map.insert dBookMapKey (updateDepthBook depthBook order) dBookMap}
    Nothing ->
      handleDepthBook
        tickerST {tickerDBookMap = Map.insert dBookMapKey (openDepthBook (getCurrencyPair order)) dBookMap}
        order
  where
    dBookMapKey = toCurrencyExchangeKey order

bufferedWrite :: TickerST -> CurrencyExchangeKey -> Maybe (Maybe Order, Maybe Order) -> Maybe DepthBook -> IO TickerST
bufferedWrite tickerST@(TickerST dbookMap tickBuffer dest queue) key (Just (lastAsk, lastBid)) (Just book)
  | (lastAsk, lastBid) /= tickPair =
    writeOutIO dest tickPair >>= (\uDest -> return (TickerST dbookMap (Map.insert key tickPair tickBuffer) uDest queue))
  | otherwise = return tickerST
  where
    minAsk = fmap snd (Map.lookupMin (depthBookAsk book))
    maxBid = fmap snd (Map.lookupMax (depthBookBid book))
    tickPair = (minAsk, maxBid)
bufferedWrite tickerST@(TickerST dbookMap tickBuffer dest queue) key Nothing (Just book) =
  writeOutIO dest tickPair >>= (\uDest -> return (TickerST dbookMap (Map.insert key tickPair tickBuffer) uDest queue))
  where
    minAsk = fmap snd (Map.lookupMin (depthBookAsk book))
    maxBid = fmap snd (Map.lookupMax (depthBookBid book))
    tickPair = (minAsk, maxBid)
bufferedWrite tickerST key lastTick book = return tickerST

decodeOrders :: Either KafkaClientError [BS.ByteString] -> IO [Order]
decodeOrders (Right msg) = return (foldl filteredDecode [] msg) :: IO [Order]
decodeOrders (Left err)  = fail "vla"

filteredDecode :: [Order] -> BS.ByteString -> [Order]
filteredDecode orders bsMessage =
  case decode (BL.fromStrict bsMessage) :: Maybe [Order] of
    (Just order) -> order ++ orders
    Nothing      -> orders
