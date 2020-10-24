{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}

module Finance.OrderBookGroup
  ( OrderBookGroup
  , ToOrderBookGroupKey
  , createOrderBookGroup
  , toOrderBookGroupKey
  , unOrderBookGroup
  , updateOrderBookInGroup
  , withOrderBook
  ) where

import qualified Data.Map          as Map
import           Finance.Currency
import           Finance.Exchange
import           Finance.Order
import           Finance.OrderBook

newtype OrderBookGroup a =
  OrderBookGroup (Map.Map a OrderBook)

class (Ord a) =>
      ToOrderBookGroupKey a
  where
  toOrderBookGroupKey :: BaseOrder -> a

createOrderBookGroup :: OrderBookGroup a
createOrderBookGroup = OrderBookGroup Map.empty

updateOrderBookInGroup :: (ToOrderBookGroupKey a) => OrderBookGroup a -> BaseOrder -> OrderBookGroup a
updateOrderBookInGroup (OrderBookGroup orderBooks) order =
  case mbOrderBook of
    (Just orderBook) -> OrderBookGroup (Map.insert orderKey (updateOrderBook orderBook order) orderBooks)
    Nothing -> updateOrderBookInGroup (OrderBookGroup newBookAdded) order
  where
    orderKey = toOrderBookGroupKey order
    mbOrderBook = Map.lookup orderKey orderBooks
    newBookAdded = Map.insert orderKey (openOrderBook exchange currency) orderBooks
    currency = orderCurrencyPair order
    exchange = orderExchange order

withOrderBook ::
     (ToOrderBookGroupKey a) => OrderBookGroup a -> a -> (OrderBook -> IO OrderBook) -> IO (OrderBookGroup a)
withOrderBook (OrderBookGroup orderBooks) orderBookKey fnToApply
  | (Just orderBook) <- Map.lookup orderBookKey orderBooks =
    fnToApply orderBook >>= (\updatedBook -> pure (OrderBookGroup (Map.insert orderBookKey updatedBook orderBooks)))
  | otherwise = pure (OrderBookGroup orderBooks)

unOrderBookGroup :: OrderBookGroup a -> Map.Map a OrderBook
unOrderBookGroup (OrderBookGroup orderBooks) = orderBooks
