module Main where

import qualified Control.Concurrent.MVar as MVar
import Finance.Types
import Exchange.Binance.Types
import Exchange.Bitstamp.Types
import Exchange.Bitstamp.Utils as Bitstamp
import Exchange.Binance.Utils as Binance
import Exchange.Kraken.Utils as Kraken
import Control.Concurrent.Chan as Chan
import Control.Concurrent

data FeeTable = BitstampFeeTable | BinanceFeeTable deriving Show

main :: IO ()
main = do
--       orderQueue <- newChan
       putStrLn "hey"
--       Bitstamp.subscribe2 orderQueue

--       bitstampFees <- newEmptyMVar
--       binanceFees <- newEmptyMVar
--       Bitstamp.subscribeToFees bitstampFees
--       Binance.subscribeToFees binanceFees
--
       orderQueue <- newChan
--       Bitstamp.subscribeToDepthBook orderQueue
--       Binance.subscribeToDepthBook orderQueue
       Kraken.subscribeToDepthBook orderQueue
--
       worker orderQueue
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
worker :: Chan [Order] -> IO ()
worker queue = loop
          where loop = do
                        orders <- Chan.readChan queue
                        putStrLn $ show orders
                        loop