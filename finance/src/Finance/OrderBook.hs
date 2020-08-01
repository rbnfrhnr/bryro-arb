{-# LANGUAGE DataKinds #-}

module Finance.OrderBook
  ( OrderBook(..)
  , OrderBookCollection(..)
  , OrderBookGroup(..)
  , OrderKey(..)
  , OrderBookKey(..)
  , getHigherBids
  , getLowerAsks
  , getOrderBookKey
  , getTick
  , openGroup
  , openOrderBook
  , unOrderBookGroup
  , updateOrderBook
  , updateOrderBookGroup
  ) where

import qualified Data.Map                   as Map

import           Finance.Currency
import           Finance.Exchange
import           Finance.Order
import           Finance.OrderBook.Internal
import           Finance.Tick

{- | This function updates a given orderBook by the order provided
     If the order is already in the book. the order gets replaced/updated
     If the order is not yet in the book, it will be added.
     If the order is in the book but the quantity of the order is zero, we delete it.

     The function works for Bid Orders as well as for Ask Orders

-}
updateOrderBook :: OrderBook -> BaseOrder -> OrderBook
updateOrderBook book order
  | (Just order) <- maybeAskOrder = updateAskOrderBook book order
  | (Just order) <- maybeBidOrder = updateBidOrderBook book order
  where
    maybeAskOrder = toAskOrder order
    maybeBidOrder = toBidOrder order

updateBidOrderBook :: OrderBook -> Order BidOrder -> OrderBook
updateBidOrderBook (OrderBook currency exchange askBook bidBook) bidOrder
  | qty > 0.0 = OrderBook currency exchange askBook (Map.insert orderKey bidOrder bidBook)
  | otherwise = OrderBook currency exchange askBook (Map.delete orderKey bidBook)
  where
    order = unBidOrder bidOrder
    orderKey = toOrderKey order
    qty = orderQuantity order

updateAskOrderBook :: OrderBook -> Order AskOrder -> OrderBook
updateAskOrderBook (OrderBook currency exchange askBook bidBook) askOrder
  | qty > 0.0 = OrderBook currency exchange (Map.insert orderKey askOrder askBook) bidBook
  | otherwise = OrderBook currency exchange (Map.delete orderKey askBook) bidBook
  where
    order = unAskOrder askOrder
    orderKey = toOrderKey order
    qty = orderQuantity order

{- | This function returns all the Ask prices which are lower than the provided Bid price for a given orderBook -}
getLowerAsks :: Order BidOrder -> OrderBook -> [Order AskOrder]
getLowerAsks order (OrderBook _ _ askBook _) = (Map.elems . fst) (Map.split (toOrderKey (unBidOrder order)) askBook)

{- | This function returns all the Bid prices which are higher than the provided asking price for a given orderBook -}
getHigherBids :: Order AskOrder -> OrderBook -> [Order BidOrder]
getHigherBids order (OrderBook _ _ _ bidBook) = (Map.elems . snd) (Map.split (toOrderKey (unAskOrder order)) bidBook)

{- | Constructor function for a orderBook -}
openOrderBook :: Exchange -> CurrencyPair -> OrderBook
openOrderBook exchange currencyPair = OrderBook currencyPair exchange Map.empty Map.empty

toOrderKey :: BaseOrder -> OrderKey
toOrderKey order = OrderKey (orderCurrentPrice order)

getTick :: OrderBook -> Tick
getTick (OrderBook currency exchange asks bids) = Tick askTick bidTick currency exchange timestamp
  where
    maybeAsk = fmap snd (Map.lookupMin asks)
    maybeBid = fmap snd (Map.lookupMax bids)
    timestamp = timestampOrDefault maybeAsk maybeBid
    askTick = lookupDefault (fmap unAskOrder maybeAsk)
    bidTick = lookupDefault (fmap unBidOrder maybeBid)

lookupDefault :: Maybe BaseOrder -> (Double, Double)
lookupDefault (Just (BaseOrder _ _ price qty _ _)) = (price, qty)
lookupDefault Nothing                              = (0, 0)

timestampOrDefault :: Maybe (Order AskOrder) -> Maybe (Order BidOrder) -> Int
timestampOrDefault (Just order) Nothing = orderTimestamp (unAskOrder order)
timestampOrDefault Nothing (Just order) = orderTimestamp (unBidOrder order)
timestampOrDefault Nothing Nothing = 0
timestampOrDefault (Just askOrder) (Just bidOrder)
  | askTime >= bidTime = askTime
  | otherwise = bidTime
  where
    askTime = orderTimestamp (unAskOrder askOrder)
    bidTime = orderTimestamp (unBidOrder bidOrder)

createOrderBookKey :: BaseOrder -> OrderBookKey
createOrderBookKey order = OrderBookKey (orderExchange order) (orderCurrencyPair order)

getOrderBookKey :: OrderBook -> OrderBookKey
getOrderBookKey orderBook = OrderBookKey (orderBookExchange orderBook) (orderBookCurrencyPair orderBook)

openGroup :: OrderBookGroup
openGroup = OrderBookGroup Map.empty

orderBooksByExchange :: OrderBookGroup -> Exchange -> OrderBookGroup
orderBooksByExchange (OrderBookGroup orderBooks) exchange =
  OrderBookGroup $ Map.filter (\orderBook -> orderBookExchange orderBook == exchange) orderBooks

orderBooksByCurrency :: OrderBookGroup -> CurrencyPair -> OrderBookGroup
orderBooksByCurrency (OrderBookGroup orderBooks) currency =
  OrderBookGroup $ Map.filter (\orderBook -> orderBookCurrencyPair orderBook == currency) orderBooks

orderBookByOrder :: OrderBookGroup -> BaseOrder -> Maybe OrderBook
orderBookByOrder (OrderBookGroup orderBooks) order = Map.lookup (createOrderBookKey order) orderBooks

updateOrderBookGroup :: OrderBookGroup -> BaseOrder -> OrderBookGroup
updateOrderBookGroup (OrderBookGroup orderBooks) order
  | (Just orderBook) <- Map.lookup orderBookKey orderBooks =
    OrderBookGroup (Map.insert orderBookKey (updateOrderBook orderBook order) orderBooks)
  | otherwise = updateOrderBookGroup (OrderBookGroup (Map.insert orderBookKey newOrderBook orderBooks)) order
  where
    orderBookKey = createOrderBookKey order
    newOrderBook = openOrderBook (orderExchange order) (orderCurrencyPair order)

withOrderBooks :: OrderBookGroup -> (OrderBook -> OrderBook) -> OrderBookGroup
withOrderBooks (OrderBookGroup orderBooks) fn =
  OrderBookGroup (foldl (\books book -> Map.insert (getOrderBookKey book) (fn book) books) orderBooks orderBooks)

unOrderBookGroup :: OrderBookGroup -> Map.Map OrderBookKey OrderBook
unOrderBookGroup (OrderBookGroup orderBooks) = orderBooks
