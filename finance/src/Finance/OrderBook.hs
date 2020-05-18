{-# LANGUAGE DataKinds #-}

module Finance.OrderBook
  ( OrderBook(..)
  , OrderBookCollection(..)
  , OrderKey(..)
  , getHigherBids
  , getLowerAsks
  , getTick
  , openOrderBook
  , updateDepthBook
  ) where

import qualified Data.Map                   as Map

import           Finance.Currency
import           Finance.Exchange
import           Finance.Order
import           Finance.OrderBook.Internal
import           Finance.Tick

{- $setup
  >>> :set -XDataKinds
  >>> import Data.Map as Map
-}
{- | This function updates a given DepthBook by the order provided
     If the order is already in the book. the order gets replaced/updated
     If the order is not yet in the book, it will be added.
     If the order is in the book but the quantity of the order is zero, we delete it.

     The function works for Bid Orders as well as for Ask Orders

-}
updateDepthBook :: OrderBook -> BaseOrder -> OrderBook
updateDepthBook book order
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

{- | This function returns all the Ask prices which are lower than the provided Bid price for a given DepthBook -}
getLowerAsks :: Order BidOrder -> OrderBook -> [Order AskOrder]
getLowerAsks order (OrderBook _ _ askBook _) = (Map.elems . fst) (Map.split (toOrderKey (unBidOrder order)) askBook)

{- | This function returns all the Bid prices which are higher than the provided asking price for a given DepthBook -}
getHigherBids :: Order AskOrder -> OrderBook -> [Order BidOrder]
getHigherBids order (OrderBook _ _ _ bidBook) = (Map.elems . snd) (Map.split (toOrderKey (unAskOrder order)) bidBook)

{- | Constructor function for a Depthbook -}
openOrderBook :: Exchange -> CurrencyPair -> OrderBook
openOrderBook exchange currencyPair = OrderBook currencyPair exchange Map.empty Map.empty

toOrderKey :: BaseOrder -> OrderKey
toOrderKey order = OrderKey (orderCurrentPrice order)

{- |
  >>> mbAsk = toAskOrder (BaseOrder Bitstamp LTCUSD 10.25 1.42 150 Ask) :: Maybe (Order AskOrder)
  >>> mbBid = toBidOrder (BaseOrder Bitstamp LTCUSD 10.2 3.0 152 Bid) :: Maybe (Order BidOrder)

  Testing empty book

  >>> let emptyAsks = Map.fromList []
  >>> let emptyBids = Map.fromList []
  >>> let emptyBook = OrderBook LTCUSD Bitstamp emptyAsks emptyBids
  >>> (Tick (0.0, 0.0) (0.0, 0.0) LTCUSD Bitstamp 0) == getTick emptyBook
  True

  Test that the tick sets default values for non present orders

  >>> :{
   case mbAsk of
    (Just ask) -> do
      let asks = Map.fromList [(toOrderKey (unAskOrder ask), ask)]
      let bids = Map.fromList []
      let book = OrderBook LTCUSD Bitstamp asks bids
      (Tick (10.25,1.42) (0.0, 0.0) LTCUSD Bitstamp 150) == getTick book
  :}
  True

  test that the tick is correctly created when both orders are present
  and the timestamp refers to the latest (higher value)

  >>> :{
   case (mbAsk, mbBid) of
     (Just ask, Just bid) -> do
       let asks = Map.fromList [(toOrderKey (unAskOrder ask), ask)]
       let bids = Map.fromList [(toOrderKey (unBidOrder bid), bid)]
       let book = OrderBook LTCUSD Bitstamp asks bids
       (Tick (10.25,1.42) (10.2, 3.0) LTCUSD Bitstamp 152) == getTick book
  :}
  True

-}
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
