module Finance.Arbitrage.Utils
  (
  ) where

import qualified Data.Map                as Map

import           Finance.Arbitrage.Types
import           Finance.OrderBook.Types
import           Finance.OrderBook.Utils as DepthBook
import           Finance.Types

getSpreadKey :: Spread -> SpreadKey
getSpreadKey (Spread (BidOrder order) _) = show exchange ++ show symbol ++ show price
  where
    exchange = orderExchange order
    symbol = orderCurrencyPair order
    price = orderCurrentPrice order

{- | this function converts a BidOrder into a SpreadMessage if there are AskOrders which's prices are lower than
     the provided bid price.-}
bidOrderToSpreadMessage :: Order -> SpreadBook -> Maybe DepthBook -> Maybe SpreadMessage
bidOrderToSpreadMessage (BidOrder order) spreadBook (Just depthBook)
  | not spreadIsInMap && not (null lowerAskOrders) = Just $ SpreadOpening spread
  | spreadIsInMap && not (null lowerAskOrders) = Just $ SpreadUpdate spread
  | spreadIsInMap && null lowerAskOrders = Just $ SpreadClosing spread
  | otherwise = Nothing
  where
    askBook = depthBookAsk depthBook
    lowerAskOrders = DepthBook.getLowerAsks (BidOrder order) depthBook
    spread = Spread (BidOrder order) lowerAskOrders
    spreadKey = getSpreadKey spread
    spreadIsInMap = Map.member spreadKey spreadBook
bidOrderToSpreadMessage _ _ _ = Nothing

orderToSpreadMessage :: Order -> SpreadBook -> Maybe DepthBook -> [Maybe SpreadMessage]
orderToSpreadMessage (BidOrder order) spreadBook maybeDepthBook =
  [bidOrderToSpreadMessage (BidOrder order) spreadBook maybeDepthBook]
orderToSpreadMessage (AskOrder order) spreadBook (Just depthBook) =
  foldl
    (\spreadMessages bidOrder -> spreadMessages ++ [bidOrderToSpreadMessage bidOrder spreadBook (Just depthBook)])
    []
    higherBids
  where
    higherBids = DepthBook.getHigherBids (AskOrder order) depthBook
orderToSpreadMessage _ _ _ = []
