{-# LANGUAGE DataKinds #-}

module Finance.Arbitrage
  ( Spread(..)
  , SpreadBook
  , SpreadKey
  , SpreadMessage(..)
  ) where

import qualified Data.Map                   as Map

import           Control.Monad              (foldM)
import           Finance.Arbitrage.Internal
import           Finance.Order
import           Finance.OrderBook

toSpreadKey :: Spread -> SpreadKey
toSpreadKey (Spread bidOrder _) = show exchange ++ show symbol ++ show price
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
bidOrderToSpreadMessage :: Order BidOrder -> SpreadBook -> OrderBook -> (SpreadBook, Maybe SpreadMessage)
bidOrderToSpreadMessage bidOrder spreadBook orderBook
  | not spreadIsInMap && not (null lowerAskOrders) = (updatedSpreadBook, Just $ SpreadOpening spread)
  | spreadIsInMap && not (null lowerAskOrders) = (updatedSpreadBook, Just $ SpreadUpdate spread)
  | spreadIsInMap && null lowerAskOrders = (removedSpreadFromBook, Just $ SpreadClosing spread)
  | otherwise = (spreadBook, Nothing)
  where
    askBook = orderBookAsk orderBook
    lowerAskOrders = getLowerAsks bidOrder orderBook
    spread = Spread bidOrder lowerAskOrders
    spreadKey = toSpreadKey spread
    spreadIsInMap = Map.member spreadKey spreadBook
    updatedSpreadBook = Map.insert spreadKey spread spreadBook
    removedSpreadFromBook = Map.delete spreadKey spreadBook

{- | If we are given an asking order, we fetch all the higher bid orders and create a spread message based on them -}
askOrderToSpreadMessage :: Order AskOrder -> SpreadBook -> OrderBook -> (SpreadBook, Maybe [SpreadMessage])
askOrderToSpreadMessage askOrder spreadBook orderBook =
  foldM
    (\(spreadBook, list) bidOrder ->
       updateTuple (spreadBook, list) (bidOrderToSpreadMessage bidOrder spreadBook orderBook))
    (spreadBook, [])
    higherBids
  where
    higherBids = getHigherBids askOrder orderBook
    updateTuple (_, oldLIst) (newBook, newList) = (newBook, oldLIst ++ newList) :: (SpreadBook, Maybe [SpreadMessage])

orderToSpreadMessage :: BaseOrder -> SpreadBook -> OrderBook -> (SpreadBook, Maybe [SpreadMessage])
orderToSpreadMessage order spreadBook orderBook
  | (Just ord) <- maybeBidOrder = fmap (: []) (bidOrderToSpreadMessage ord spreadBook orderBook)
  | (Just ord) <- maybeAskOrder = askOrderToSpreadMessage ord spreadBook orderBook
  where
    maybeBidOrder = toBidOrder order
    maybeAskOrder = toAskOrder order
