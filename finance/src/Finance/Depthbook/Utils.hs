module Finance.Depthbook.Utils
       (
         getHigherBids
        ,getLowerAsks
        ,updateDepthBook
        ,openDepthBook
       ) where

import Finance.Types
import Finance.Depthbook.Types
import qualified Data.Map as Map

{- | This function updates a given DepthBook by the order provided
     If the order is already in the book. the order gets replaced/updated
     If the order is not yet in the book, it will be added.
     If the order is in the book but the quantity of the order is zero, we delete it.

     The function works for Bid Orders as well as for Ask Orders
-}
updateDepthBook :: DepthBook -> Order -> DepthBook
updateDepthBook book@(DepthBook currencyPair askbook bidbook) order@(AskOrder baseOrder)
        | qty >  0.0 = DepthBook currencyPair orderInsertedAskBook bidbook
        | qty == 0.0 = DepthBook currencyPair orderDeletedAskBook bidbook
            where orderDeletedAskBook  = Map.delete order askbook
                  orderInsertedAskBook = Map.insert order order askbook
                  qty                  = orderQuantity baseOrder
updateDepthBook book@(DepthBook currencyPair askbook bidbook) order@(BidOrder baseOrder)
        | qty >  0.0 =  DepthBook currencyPair askbook orderInsertedBidBook
        | qty == 0.0 =  DepthBook currencyPair askbook orderDeletedBidBook
            where orderInsertedBidBook = Map.insert order order bidbook
                  orderDeletedBidBook  = Map.delete order bidbook
                  qty                  = orderQuantity baseOrder

{- | A collection and an array of Orders can be provided and the corresponding DepthBook will be updated.
     The correct DepthBook can be derived from the Order
-}
updateCollection :: DepthBookCollection -> [Order] -> DepthBookCollection
updateCollection collection (order:xs)
-- todo come up with a better way to update the collection and passing it on...
                    | (Just book) <- maybeBook = updateCollection (Map.insert (depthBookCurrencyPair (updateDepthBook book order)) (updateDepthBook book order) collection) xs
                    | otherwise                = updateCollection collection xs
                    where maybeBook    = Map.lookup currencyPair collection
                          currencyPair = getCurrencyPair order

{- | This function returns all the Ask prices which are lower than the provided Bid price for a given DepthBook -}
getLowerAsks :: Order -> DepthBook -> [Order]
getLowerAsks order@(AskOrder _) _ = []
getLowerAsks order@(BidOrder _) (DepthBook _ askBook _) = Prelude.map (fst) (Map.toList (fst (Map.split order askBook)))

{- | This function returns all the Bid prices which are higher than the provided asking price for a given DepthBook -}
getHigherBids :: Order -> DepthBook -> [Order]
getHigherBids order@(BidOrder _) _ = []
getHigherBids order@(AskOrder _) (DepthBook _ _ bidBook) = Prelude.map (fst) (Map.toList (snd (Map.split order bidBook)))

{- | Constructor function for a Depthbook -}
openDepthBook :: CurrencyPair -> DepthBook
openDepthBook currencyPair = DepthBook currencyPair Map.empty Map.empty

{- | Constructor function for DepthBook-Collection -}
createDepthBookCollection :: DepthBookCollection
createDepthBookCollection = Map.empty

{- | Add a DepthBook to an existing DepthBook-Collection -}
addDepthBookToCollection :: DepthBookCollection -> DepthBook -> CurrencyPair -> DepthBookCollection
addDepthBookToCollection collection depthBookToAdd bookCurrencyPair = Map.insert bookCurrencyPair depthBookToAdd collection

{- | Get a DepthBook from a collection by the CurrencyPair-}
getDepthBookFromCollection :: DepthBookCollection -> CurrencyPair -> Maybe DepthBook
getDepthBookFromCollection collection bookCurrencyPair = Map.lookup bookCurrencyPair collection
