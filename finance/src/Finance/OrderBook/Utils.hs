{-# LANGUAGE DataKinds #-}

module Finance.OrderBook.Utils
  ( getHigherBids
  , getLowerAsks
  , updateDepthBook
  , openOrderBook
  , getTick
  ) where

import qualified Data.Map                as Map

import           Finance.OrderBook.Types
import           Finance.Types
import           Finance.Utils

{- $setup
 >>> import           Test.QuickCheck
 >>> import           Test.QuickCheck.Gen (choose)
 >>> instance Arbitrary BaseOrder where arbitrary = BaseOrder <$> return Bitstamp <*> return LTCUSD <*> choose (9, 10) <*> choose (0, 10) <*> return 0
 -}
{- | This function updates a given DepthBook by the order provided
     If the order is already in the book. the order gets replaced/updated
     If the order is not yet in the book, it will be added.
     If the order is in the book but the quantity of the order is zero, we delete it.

     The function works for Bid Orders as well as for Ask Orders

     >>> putStrLn "hey"
     hey

     >>> 1 + 3
     4

     prop> \(BaseOrder exc cur price qty ts) -> exc == Bitstamp && cur == LTCUSD && (price >= 9 && price <= 10) && (qty >= 0 && qty <= 10)
-}
updateDepthBook :: OrderBook -> BaseOrder -> OrderBook
updateDepthBook book order
  | (Just order) <- maybeAskOrder = updateAskOrderBook book order
  | (Just order) <- maybeBidOrder = updateBidOrderBook book order
  where
    maybeAskOrder = toAskOrder order
    maybeBidOrder = toBidOrder order

updateBidOrderBook :: OrderBook -> Order BidOrder -> OrderBook
updateBidOrderBook (OrderBook currency askBook bidBook) bidOrder
  | qty > 0.0 = OrderBook currency askBook (Map.insert orderKey bidOrder bidBook)
  | otherwise = OrderBook currency askBook (Map.delete orderKey bidBook)
  where
    order = unBidOrder bidOrder
    orderKey = toOrderKey order
    qty = orderQuantity order

updateAskOrderBook :: OrderBook -> Order AskOrder -> OrderBook
updateAskOrderBook (OrderBook currency askBook bidBook) askOrder
  | qty > 0.0 = OrderBook currency (Map.insert orderKey askOrder askBook) bidBook
  | otherwise = OrderBook currency (Map.delete orderKey askBook) bidBook
  where
    order = unAskOrder askOrder
    orderKey = toOrderKey order
    qty = orderQuantity order

{- | This function returns all the Ask prices which are lower than the provided Bid price for a given DepthBook -}
getLowerAsks :: Order BidOrder -> OrderBook -> [Order AskOrder]
getLowerAsks order (OrderBook _ askBook _) = (Map.elems . fst) (Map.split (toOrderKey (unBidOrder order)) askBook)

{- | This function returns all the Bid prices which are higher than the provided asking price for a given DepthBook -}
getHigherBids :: Order AskOrder -> OrderBook -> [Order BidOrder]
getHigherBids order (OrderBook _ _ bidBook) = (Map.elems . snd) (Map.split (toOrderKey (unAskOrder order)) bidBook)

{- | Constructor function for a Depthbook -}
openOrderBook :: CurrencyPair -> OrderBook
openOrderBook currencyPair = OrderBook currencyPair Map.empty Map.empty

toOrderKey :: BaseOrder -> OrderKey
toOrderKey order = OrderKey (orderCurrentPrice order)

{- todo implement function. because of refactoring it has been removed/modified but others still rely on it -}
getTick :: OrderBook -> Tick
getTick book = Tick Nothing Nothing Nothing Nothing 0
