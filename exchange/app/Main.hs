module Main where

import qualified Control.Concurrent.MVar as MVar

import           Control.Concurrent
import           Control.Concurrent.Chan as Chan
import           Exchange.Binance        as Binance
import           Exchange.Bitstamp       as Bitstamp
import           Exchange.Kraken         as Kraken
import           Exchange.Types
import           Exchange.Utils
import           System.IO

main :: IO ()
main = do
  orderQueue <- newChan
  putStrLn "Starting to subscribe to orderBooks" >> hFlush stdout
  subscribeOrderBook Kraken.new (Chan.writeChan orderQueue)
  subscribeOrderBook Bitstamp.new (Chan.writeChan orderQueue)
  subscribeOrderBook Binance.new (Chan.writeChan orderQueue)
  worker orderQueue

worker :: (Show a) => Chan a -> IO ()
worker queue = Chan.readChan queue >>= print >> hFlush stdout >> worker queue
