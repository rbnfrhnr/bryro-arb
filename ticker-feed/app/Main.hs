{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent.Chan as Chan
import qualified Data.Map                as Map

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Configurator
import           Data.Configurator.Types
import           Exchange
import           Exchange.Types          (ExchangeAdapterImpl (..),
                                          subscribeOrderBook)
import           Finance
import           System.FilePath
import           System.IO
import           Ticker.Utils
import           Utils.Forward
import           Utils.Influx            as Influx
import           Utils.Kafka             as Kafka

configFileKafka :: IO (Either SomeException Config)
configFileKafka = try $ load [Required $ "resources" </> "config.cfg"]

main :: IO ()
main = configFileKafka >>= withConfig

withConfig :: Either SomeException Config -> IO ()
withConfig (Right cfg) = do
  orderQueue <- Chan.newChan
  _ <- subscribeOrderBookAll orderQueue
  kafkaConfig <- Kafka.createKafkaConfig cfg
  influxHandle <- Influx.new cfg :: IO Influx.InfluxHandle
  runTransform
    (TickerHandle
       openGroup
       Map.empty
       [Destination (writeHandle kafkaConfig "bryro-ticker" 0), Destination influxHandle]
       orderQueue)

runTransform :: TickerHandle -> IO ()
runTransform tickerHandle@(TickerHandle dBookMap tBuffer destis queue) =
  Chan.readChan queue >>=
  (\orders -> pure (TickerHandle (foldl updateOrderBookGroup dBookMap orders) tBuffer destis queue)) >>=
  (\tickerHandleUpdated ->
     foldM
       (\tickerHandle' orderBook -> handleTickAndWriteIO orderBook tickerHandle')
       tickerHandleUpdated
       (unOrderBookGroup (tickerDBookGroup tickerHandleUpdated))) >>=
  runTransform >>
  return ()

handleTickAndWriteIO :: OrderBook -> TickerHandle -> IO TickerHandle
handleTickAndWriteIO orderBook tickerHandle@(TickerHandle orderBookGroup tickerBuffer destis queue)
  | Just tick /= mbExistingTick =
    writeOutIO destis tick >>= (\destis' -> pure (TickerHandle orderBookGroup tickerBuffer' destis' queue))
  | otherwise = pure tickerHandle
  where
    tick = getTick orderBook
    orderBookKey = OrderBookKey (tickExchange tick) (tickCurrency tick)
    mbExistingTick = Map.lookup orderBookKey tickerBuffer
    tickerBuffer' = Map.insert orderBookKey tick tickerBuffer
