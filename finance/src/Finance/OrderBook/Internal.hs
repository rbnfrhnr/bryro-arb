{-# LANGUAGE DataKinds #-}

module Finance.OrderBook.Internal
  ( OrderBook(..)
  , OrderKey(..)
  ) where

import           Data.Map
import           Finance.Currency
import           Finance.Exchange
import           Finance.Order

{- | This Data-Structure keeps track of all the Ask and Bid prices of a certain Currency Pair.
     It holds prices regardless of their source exchange. It's ordered by Currency Pair.
     A valid CurrencyPair could be LTCUSD
-}
data OrderBook =
  OrderBook
    { orderBookCurrencyPair :: !CurrencyPair -- ^ Identifier for this orderBook. (LTCUSD, XRPUSD etc)
    , orderBookExchange     :: Exchange
    , orderBookAsk          :: Map OrderKey (Order AskOrder) -- ^ Collection of asking prices for this CurrencyPair
    , orderBookBid          :: Map OrderKey (Order BidOrder) -- ^ Collection of biding prices fot this CurrencyPair
    }
  deriving (Show)

newtype OrderKey =
  OrderKey Double
  deriving (Show)

instance Eq OrderKey where
  (==) (OrderKey price1) (OrderKey price2) = price1 == price2

instance Ord OrderKey where
  compare (OrderKey price1) (OrderKey price2) = compare price1 price2
