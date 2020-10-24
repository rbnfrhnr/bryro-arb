{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Main where

import qualified Control.Concurrent.Chan as Chan
import           Control.Monad           (foldM)
import qualified Data.Map                as Map
import           Exchange
import           Finance
import           Finance.Arbitrage
import           Spread.Utils
import           System.IO               (hFlush, stdout)
import           Utils.Forward


instance ToOrderBookGroupKey CurrencyPair where
  toOrderBookGroupKey order = orderCurrencyPair order

data SpreadHandle =
  SpreadHandle
    { spreadHandleDestinations        :: [Destination SpreadMessage]
    , spreadHandleOrderBookCollection :: OrderBookGroup CurrencyPair
    , spreadHandleSpreadBook          :: SpreadBook
    , spreadHandleOrderQueue          :: Chan.Chan [BaseOrder]
    }

new :: IO SpreadHandle
new = do
  destis <- createDestinations
  orderQueue <- Chan.newChan
  _ <- subscribeOrderBookAll orderQueue
  pure $ SpreadHandle destis orderBookCollection spreadBook orderQueue
  where
    orderBookCollection = createOrderBookGroup :: OrderBookGroup CurrencyPair
    spreadBook = Map.empty :: Map.Map SpreadKey Spread

main :: IO ()
main = new >>= run

run :: SpreadHandle -> IO ()
run handle@(SpreadHandle destis books spreads orderQueue) = do
  orders <- Chan.readChan orderQueue
  handle' <- foldM handleSpread handle orders
  _ <- run handle'
  pure ()

handleSpread :: SpreadHandle -> BaseOrder -> IO SpreadHandle
handleSpread handle@(SpreadHandle destis books spreads orderQueue) order
  | (Just book) <- Map.lookup currencyPair (unOrderBookGroup updatedBook) =
    foldM withSpreadMsg (SpreadHandle destis updatedBook spreads orderQueue) (orderToSpreadMessage order spreads book)
  where
    updatedBook = updateOrderBookInGroup books order
    currencyPair = orderCurrencyPair order

withSpreadMsg :: SpreadHandle -> SpreadMessage -> IO SpreadHandle
withSpreadMsg (SpreadHandle destis books spreads orderQueue) msg = do
  let spreads' = updateSpreadBook spreads msg
  destis' <- writeOutIO destis msg
  pure (SpreadHandle destis' books spreads' orderQueue)
