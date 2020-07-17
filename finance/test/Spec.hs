import           Finance
import           Generators
import           Test.DocTest
import           Test.QuickCheck
import           Test.QuickCheck.Gen (choose)

main :: IO ()
main = quickCheck prop_orderBookUpdateTypes >> quickCheck prop_orderBookUpdateQtys

--main = quickCheck checkMyTest >> doctest ["-isrc", "src/Finance/OrderBook/Utils.hs"]
checkMyTest :: BaseOrder -> Bool
checkMyTest (BaseOrder exc cur price qty ts Ask) =
  exc == Bitstamp && cur == LTCUSD && (price >= 9 && price <= 10) && (qty >= 0 && qty <= 10)

prop_orderBookUpdateTypes :: [BaseOrder] -> Bool
prop_orderBookUpdateTypes orders =
  all (\ask -> orderType (unAskOrder ask) == Ask) (orderBookAsk updatedBook) &&
  all (\bid -> orderType (unBidOrder bid) == Bid) (orderBookBid updatedBook)
  where
    currency = LTCUSD
    exchange = Bitstamp
    emptyBook = openOrderBook exchange currency
    updatedBook = foldl updateDepthBook emptyBook orders

prop_orderBookUpdateQtys :: [BaseOrder] -> Bool
prop_orderBookUpdateQtys orders =
  all (\ask -> orderQuantity (unAskOrder ask) /= 0.0) (orderBookAsk updatedBook) &&
  all (\bid -> orderQuantity (unBidOrder bid) /= 0.0) (orderBookBid updatedBook)
  where
    currency = LTCUSD
    exchange = Bitstamp
    emptyBook = openOrderBook exchange currency
    updatedBook = foldl updateDepthBook emptyBook orders
