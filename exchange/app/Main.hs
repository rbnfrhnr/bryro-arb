module Main where

import qualified Control.Concurrent.MVar as MVar

import           Control.Concurrent
import           Control.Concurrent.Chan as Chan
import           Exchange.Binance.Utils  as Binance
import           Exchange.Bitstamp.Utils as Bitstamp
import           Exchange.Handler
import           Exchange.Kraken.Utils   as Kraken
import           Exchange.Utils
import           System.IO

main :: IO ()
main = do
  orderQueue <- newChan
  putStrLn "Starting to subscribe to orderBooks" >> hFlush stdout
  Bitstamp.subscribeHandler $ decodeAndEnQueueHandler Bitstamp.parseToOrder orderQueue
  Kraken.subscribeHandler $ decodeAndEnQueueHandler Kraken.parseToOrder orderQueue
  Binance.subscribeHandler $ decodeAndEnQueueHandler Binance.parseToOrder orderQueue
  worker orderQueue

worker :: (Show a) => Chan a -> IO ()
worker queue = Chan.readChan queue >>= print >> hFlush stdout >> worker queue
