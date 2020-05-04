module Finance.OrderBook.Utils
  ( getHigherBids
  , getLowerAsks
  , getTick
  , updateDepthBook
  , updateDepthBookOrders
  , openOrderBook
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
updateDepthBook :: OrderBook -> Order -> OrderBook
updateDepthBook book@(OrderBook currencyPair askbook bidbook) order@(AskOrder baseOrder)
  | qty > 0.0 = OrderBook currencyPair orderInsertedAskBook bidbook
  | otherwise = OrderBook currencyPair orderDeletedAskBook bidbook
  where
    orderDeletedAskBook = Map.delete (toOrderKey order) askbook
    orderInsertedAskBook = Map.insert (toOrderKey order) order askbook
    qty = orderQuantity baseOrder
updateDepthBook book@(OrderBook currencyPair askbook bidbook) order@(BidOrder baseOrder)
  | qty > 0.0 = OrderBook currencyPair askbook orderInsertedBidBook
  | otherwise = OrderBook currencyPair askbook orderDeletedBidBook
  where
    orderInsertedBidBook = Map.insert (toOrderKey order) order bidbook
    orderDeletedBidBook = Map.delete (toOrderKey order) bidbook
    qty = orderQuantity baseOrder

updateDepthBookOrders :: OrderBook -> [Order] -> OrderBook
updateDepthBookOrders = foldl updateDepthBook

{- | A collection and an array of Orders can be provided and the corresponding DepthBook will be updated.
     The correct DepthBook can be derived from the Order
-}
updateCollection :: OrderBookCollection -> [Order] -> OrderBookCollection
updateCollection collection (order:xs)
  | (Just book) <- maybeBook =
    updateCollection
      (Map.insert (depthBookCurrencyPair (updateDepthBook book order)) (updateDepthBook book order) collection)
      xs
  | otherwise = updateCollection collection xs
  where
    maybeBook = Map.lookup currencyPair collection
    currencyPair = getCurrencyPair order

toOrderKey :: Order -> OrderKey
toOrderKey order = OrderKey (getPriceFromOrder order)

{- | This function returns all the Ask prices which are lower than the provided Bid price for a given DepthBook -}
getLowerAsks :: Order -> OrderBook -> [Order]
getLowerAsks order@(AskOrder _) _ = []
getLowerAsks order@(BidOrder _) (OrderBook _ askBook _) = (Map.elems . fst) (Map.split (toOrderKey order) askBook)

{- | This function returns all the Bid prices which are higher than the provided asking price for a given DepthBook -}
getHigherBids :: Order -> OrderBook -> [Order]
getHigherBids order@(BidOrder _) _ = []
getHigherBids order@(AskOrder _) (OrderBook _ _ bidBook) = (Map.elems . snd) (Map.split (toOrderKey order) bidBook)

{- | Constructor function for a Depthbook -}
openOrderBook :: CurrencyPair -> OrderBook
openOrderBook currencyPair = OrderBook currencyPair Map.empty Map.empty

{- | Constructor function for DepthBook-Collection -}
createOrderBookCollection :: OrderBookCollection
createOrderBookCollection = Map.empty

{- | Add a DepthBook to an existing DepthBook-Collection -}
addDepthBookToCollection :: OrderBookCollection -> OrderBook -> CurrencyPair -> OrderBookCollection
addDepthBookToCollection collection depthBookToAdd bookCurrencyPair =
  Map.insert bookCurrencyPair depthBookToAdd collection

{- | Get a DepthBook from a collection by the CurrencyPair-}
getDepthBookFromCollection :: OrderBookCollection -> CurrencyPair -> Maybe OrderBook
getDepthBookFromCollection collection bookCurrencyPair = Map.lookup bookCurrencyPair collection

{- | extracts a tick (lowest ask and highest bid) from a given order book.
 -}
getTick :: OrderBook -> Tick
getTick (OrderBook _ asks bids) = Tick minAsk maxBid timestamp
  where
    minAsk = fmap snd (Map.lookupMin asks)
    maxBid = fmap snd (Map.lookupMax bids)
    timestamp = getLatestTimestamp minAsk maxBid

getLatestTimestamp :: Maybe Order -> Maybe Order -> Int
getLatestTimestamp Nothing Nothing = 0
getLatestTimestamp (Just order) Nothing = getTimestampFromOrder order
getLatestTimestamp Nothing (Just order) = getTimestampFromOrder order
getLatestTimestamp (Just order1) (Just order2)
  | timestamp1 > timestamp2 = timestamp1
  | timestamp2 > timestamp1 = timestamp2
  | otherwise = timestamp1
  where
    timestamp1 = getTimestampFromOrder order1
    timestamp2 = getTimestampFromOrder order2
