module Arbitrage.Utils(
) where

import Arbitrage.Types
import Types
import Depthbook.Types
import Depthbook.Utils as DepthBook
import Data.Map as Map


getSpreadKey :: Spread -> SpreadKey
getSpreadKey (Spread (BidOrder order) _) = show (exchange) ++ show (symbol) ++ show (price)
             where exchange = orderExchange order
                   symbol   = orderCurrencyPair order
                   price    = orderCurrentPrice order

{- | this function converts a BidOrder into a SpreadMessage if there are AskOrders which's prices are lower than
     the provided bid price.-}
bidOrderToSpreadMessage :: Order -> SpreadBook -> Maybe DepthBook -> Maybe SpreadMessage
bidOrderToSpreadMessage (BidOrder order) spreadBook (Just depthBook)
                        | not spreadIsInMap && length lowerAskOrders >  0 = Just $ SpreadOpening spread
                        | spreadIsInMap     && length lowerAskOrders >  0 = Just $ SpreadUpdate spread
                        | spreadIsInMap     && length lowerAskOrders == 0 = Just $ SpreadClosing spread
                        | otherwise = Nothing
                        where askBook        = depthBookAsk depthBook
                              lowerAskOrders = DepthBook.getLowerAsks (BidOrder order) depthBook
                              spread         = Spread (BidOrder order) lowerAskOrders
                              spreadKey      = getSpreadKey spread
                              spreadIsInMap  = Map.member spreadKey spreadBook
bidOrderToSpreadMessage  _ _ _ = Nothing

orderToSpreadMessage :: Order -> SpreadBook -> Maybe DepthBook -> [Maybe SpreadMessage]
orderToSpreadMessage (BidOrder order) spreadBook maybeDepthBook = [bidOrderToSpreadMessage (BidOrder order) spreadBook maybeDepthBook]
orderToSpreadMessage (AskOrder order) spreadBook (Just depthBook) = Prelude.foldl (\ spreadMessages bidOrder -> spreadMessages ++ [(bidOrderToSpreadMessage bidOrder spreadBook (Just depthBook))]) [] higherBids
                     where higherBids = DepthBook.getHigherBids (AskOrder order) depthBook
orderToSpreadMessage _ _ _ = []
