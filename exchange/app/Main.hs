module Main where

import Finance.Types
import Exchange.Bitstamp.Utils as Bitstamp
import Exchange.Binance.Utils as Binance
import Exchange.Kraken.Utils as Kraken
import Control.Concurrent.Chan as Chan
import Control.Concurrent


main :: IO ()
main = do
       queue <- newChan
       Bitstamp.subscribeToDepthBook queue
       Binance.subscribeToDepthBook queue
       Kraken.subscribeToDepthBook queue
       worker queue
       return ()



worker :: Chan [Order] -> IO ()
worker queue = loop
          where loop = do
                        orders <- Chan.readChan queue
                        putStrLn $ show orders
                        loop