{-# LANGUAGE DataKinds #-}

module Finance.Arbitrage
  ( Spread(..)
  , SpreadBook
  , SpreadKey
  , SpreadMessage(..)
  , orderToSpreadMessage
  , updateSpreadBook
  ) where

import qualified Data.Map                   as Map

import           Control.Monad              (foldM)
import           Finance.Arbitrage.Internal
import           Finance.Order
import           Finance.OrderBook

toSpreadKey :: Order BidOrder -> SpreadKey
toSpreadKey bidOrder = show exchange ++ show symbol ++ show price
  where
    order = unBidOrder bidOrder
    exchange = orderExchange order
    symbol = orderCurrencyPair order
    price = orderCurrentPrice order

{- | this function converts a BidOrder into a SpreadMessage if there are AskOrders which's prices are lower than
     the provided bid price.

    - SpreadOpening -> there are asking orders lower in price than the bid price provided
                       and the SpreadKey is not yet in the map
    - SpreadUpdate  -> SpreadKey is already in the map and there are still asking orders lower than the bid order
    - SpreadClosing -> SpreadKey is in the map but there are no asking orders lower than the bid order
     -}
bidOrderToSpreadMessage :: Order BidOrder -> SpreadBook -> OrderBook -> [SpreadMessage]
bidOrderToSpreadMessage bidOrder spreadBook orderBook
  | not spreadIsInMap && not (null lowerAskOrders) = [SpreadOpening spread]
  | spreadIsInMap && not (null lowerAskOrders) = [SpreadUpdate spread]
  | spreadIsInMap && null lowerAskOrders = [SpreadClosing spread]
  | otherwise = []
  where
    askBook = orderBookAsk orderBook
    lowerAskOrders = getLowerAsks bidOrder orderBook
    spread = Spread spreadKey bidOrder lowerAskOrders
    spreadKey = toSpreadKey bidOrder
    spreadIsInMap = Map.member spreadKey spreadBook
    removedSpreadFromBook = Map.delete spreadKey spreadBook

{- | If we are given an asking order, we fetch all the higher bid orders and create a spread message based on them -}
askOrderToSpreadMessage :: Order AskOrder -> SpreadBook -> OrderBook -> [SpreadMessage]
askOrderToSpreadMessage askOrder spreadBook orderBook =
  foldl (\list bidOrder -> list ++ bidOrderToSpreadMessage bidOrder spreadBook orderBook) [] higherBids
  where
    higherBids = getHigherBids askOrder orderBook

orderToSpreadMessage :: BaseOrder -> SpreadBook -> OrderBook -> [SpreadMessage]
orderToSpreadMessage order spreadBook orderBook
  | (Just ord) <- maybeBidOrder = bidOrderToSpreadMessage ord spreadBook orderBook
  | (Just ord) <- maybeAskOrder = askOrderToSpreadMessage ord spreadBook orderBook
  where
    maybeBidOrder = toBidOrder order
    maybeAskOrder = toAskOrder order

updateSpreadBook :: SpreadBook -> SpreadMessage -> SpreadBook
updateSpreadBook spreadBook (SpreadOpening spread) = Map.insert (spreadKey spread) spread spreadBook
updateSpreadBook spreadBook (SpreadUpdate spread) = Map.insert (spreadKey spread) spread spreadBook
updateSpreadBook spreadBook (SpreadClosing spread) = Map.delete (spreadKey spread) spreadBook
