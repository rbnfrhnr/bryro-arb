{-# LANGUAGE DeriveGeneric #-}

module Finance.Order
  ( BaseOrder(..)
  , Order(..)
  , OrderPrice
  , OrderQty
  ) where

import           Data.Aeson       (FromJSON, ToJSON)
import           Finance.Currency
import           Finance.Exchange
import           GHC.Generics     (Generic)

type OrderPrice = Double

type OrderQty = Double

{- | Base representation of an order offered through an exchange. -}
data BaseOrder =
  BaseOrder
    { orderExchange     :: !Exchange -- ^ the exchange from which the order originated
    , orderCurrencyPair :: !CurrencyPair -- ^ the currencypair for which the order is for
    , orderCurrentPrice :: !OrderPrice -- ^ the currently offered price for this order.
    , orderQuantity     :: !OrderQty -- ^ the offered quantity for this order
    , orderTimestamp    :: !Int -- ^ the time at which this order was last updated / created
    }
  deriving (Show, Generic)

{- | An order can either be an asking or a bidding order-}
data Order
  = AskOrder BaseOrder
  | BidOrder BaseOrder
  deriving (Show, Generic)

{- | A prices equality is defined by its price and quantity -}
instance Eq BaseOrder where
  (==) (BaseOrder _ _ price qty _) (BaseOrder _ _ price2 qty2 _) = (==) price price2 && (==) qty qty2

{- | A prices equality is defined by its price and quantity -}
instance Ord BaseOrder where
  compare (BaseOrder _ _ price _ _) (BaseOrder _ _ price2 _ _) = compare price price2

instance Eq Order where
  (==) (AskOrder baseOrder) (AskOrder baseOrder2) = (==) baseOrder baseOrder2
  (==) (AskOrder baseOrder) (BidOrder baseOrder2) = (==) baseOrder baseOrder2
  (==) (BidOrder baseOrder) (AskOrder baseOrder2) = (==) baseOrder baseOrder2
  (==) (BidOrder baseOrder) (BidOrder baseOrder2) = (==) baseOrder baseOrder2

instance Ord Order where
  compare (AskOrder baseOrder) (AskOrder baseOrder2) = compare baseOrder baseOrder2
  compare (AskOrder baseOrder) (BidOrder baseOrder2) = compare baseOrder baseOrder2
  compare (BidOrder baseOrder) (AskOrder baseOrder2) = compare baseOrder baseOrder2
  compare (BidOrder baseOrder) (BidOrder baseOrder2) = compare baseOrder baseOrder2

instance ToJSON BaseOrder

instance FromJSON BaseOrder

instance ToJSON Order

instance FromJSON Order
