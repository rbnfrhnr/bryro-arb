module Finance.Utils
  ( getCurrencyPair
  , getExchangeFromOrder
  , getPriceFromOrder
  , getQtyFromOrder
  ) where

import           Finance.Types

getCurrencyPair :: Order -> CurrencyPair
getCurrencyPair (AskOrder (BaseOrder _ pair _ _ _)) = pair
getCurrencyPair (BidOrder (BaseOrder _ pair _ _ _)) = pair

getExchangeFromOrder :: Order -> Exchange
getExchangeFromOrder (AskOrder order) = orderExchange order
getExchangeFromOrder (BidOrder order) = orderExchange order

getPriceFromOrder :: Order -> OrderPrice
getPriceFromOrder (AskOrder order) = orderCurrentPrice order
getPriceFromOrder (BidOrder order) = orderCurrentPrice order

getQtyFromOrder :: Order -> OrderQty
getQtyFromOrder (AskOrder order) = orderQuantity order
getQtyFromOrder (BidOrder order) = orderQuantity order
