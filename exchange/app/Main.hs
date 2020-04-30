module Main where

import qualified Control.Concurrent.MVar as MVar

import           Control.Concurrent
import           Control.Concurrent.Chan as Chan
import           Exchange.Binance.Utils  as Binance
import           Exchange.Bitstamp.Utils as Bitstamp
import           Exchange.Kraken.Utils   as Kraken
import           Exchange.Network.Utils
import           Finance.Types
import           System.IO

main :: IO ()
main = do
  orderQueue <- newChan
  putStrLn "Starting to subscribe to depthbooks" >> hFlush stdout
  Bitstamp.subscribeToDepthBook $ defaultHandler orderQueue $ fmap (fmap show) Bitstamp.parseBitstampMessage
  Kraken.subscribeToDepthBook $ defaultHandler orderQueue $ fmap (fmap show) Kraken.parseKrakenMessage
  Binance.subscribeToDepthBook $ defaultHandler orderQueue $ fmap (fmap show) Binance.parseBinanceMessage
  worker orderQueue

worker :: (Show a) => Chan a -> IO ()
worker queue = Chan.readChan queue >>= print >> hFlush stdout >> worker queue
