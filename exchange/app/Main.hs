module Main where

import qualified Control.Concurrent.MVar as MVar
import Finance.Types
import Exchange.Network.Utils
import Exchange.Binance.Types
import Exchange.Bitstamp.Types
import Exchange.Bitstamp.Utils as Bitstamp
import Exchange.Binance.Utils as Binance
import Exchange.Kraken.Utils as Kraken
import Control.Concurrent.Chan as Chan
import Control.Concurrent

import Exchange.Network.Sync.Cluster

data FeeTable = BitstampFeeTable | BinanceFeeTable deriving Show

main :: IO ()
main = return ()
--       test2
--       orderQueue <- newChan
--       orderQueue <- newChan :: (Show a) => IO (Chan a)
--       putStrLn "hey"
--       Bitstamp.subscribe2 orderQueue

--       bitstampFees <- newEmptyMVar
--       binanceFees <- newEmptyMVar
--       Bitstamp.subscribeToFees bitstampFees
--       Binance.subscribeToFees binanceFees
--
--       (orderFeedHandler queue parseBitstampMessage)
--       Bitstamp.subscribeToDepthBook $ defaultHandler orderQueue $ fmap (fmap show) Bitstamp.parseBitstampMessage
--       Binance.subscribeToDepthBook $ defaultHandler orderQueue $ fmap (fmap show) Binance.parseBinanceMessage
--       Kraken.subscribeToDepthBook $ defaultHandler orderQueue $ fmap (fmap show)  Kraken.parseKrakenMessage
--
--       worker orderQueue
--       forkIO $ feesWorker bitstampFees
--       feesWorker binanceFees

--feesWorker :: (Show a, Fee a) => MVar.MVar a -> IO ()
--feesWorker feesHolder = loop
--          where loop = do
--                       fees <- MVar.readMVar feesHolder
--                       putStrLn $ show fees
--                       threadDelay 5000000
--                       loop
--
worker :: (Show a) => Chan a -> IO ()
worker queue = loop
          where loop = do
                        orders <- Chan.readChan queue
                        putStrLn $ show orders
                        loop