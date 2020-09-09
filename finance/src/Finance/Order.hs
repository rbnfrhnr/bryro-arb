{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

module Finance.Order
  ( Order
  , BaseOrder(..)
  , OrderPrice
  , OrderQty
  , OrderType(..)
  , RealOrder(..)
  , toAskOrder
  , toBidOrder
  , unAskOrder
  , unBidOrder
  ) where

import           Data.Aeson       (FromJSON, ToJSON)
import           Finance.Currency
import           Finance.Exchange
import           GHC.Generics     (Generic)

type OrderPrice = Double

type OrderQty = Double

data OrderType
  = Ask
  | Bid
  deriving (Show, Generic)

data RealOrder
  = AskOrder
  | BidOrder
  deriving (Show)

{- | Base representation of an order offered through an exchange. -}
newtype Order (a :: RealOrder) =
  Order BaseOrder
  deriving (Show, Generic)

instance ToJSON (Order AskOrder)

instance ToJSON (Order BidOrder)

data BaseOrder =
  BaseOrder
    { orderExchange     :: !Exchange -- ^ the exchange from which the order originated
    , orderCurrencyPair :: !CurrencyPair -- ^ the currencypair for which the order is for
    , orderCurrentPrice :: !OrderPrice -- ^ the currently offered price for this order.
    , orderQuantity     :: !OrderQty -- ^ the offered quantity for this order
    , orderTimestamp    :: !Int -- ^ the time at which this order was last updated / created
    , orderType         :: !OrderType -- ^ the time at which this order was last updated / created
    }
  deriving (Show, Generic)

{- | A prices equality is defined by its price and quantity -}
instance Eq BaseOrder where
  (==) (BaseOrder _ _ price qty _ _) (BaseOrder _ _ price2 qty2 _ _) = (==) price price2 && (==) qty qty2

{- | A prices equality is defined by its price and quantity -}
instance Ord BaseOrder where
  compare (BaseOrder _ _ price _ _ _) (BaseOrder _ _ price2 _ _ _) = compare price price2

instance ToJSON OrderType

instance FromJSON OrderType

instance ToJSON BaseOrder

instance FromJSON BaseOrder

toBidOrder :: BaseOrder -> Maybe (Order BidOrder)
toBidOrder order@(BaseOrder _ _ _ _ _ Bid) = Just (Order order)
toBidOrder _                               = Nothing

toAskOrder :: BaseOrder -> Maybe (Order AskOrder)
toAskOrder order@(BaseOrder _ _ _ _ _ Ask) = Just (Order order)
toAskOrder _                               = Nothing

unAskOrder :: Order AskOrder -> BaseOrder
unAskOrder (Order order) = order

unBidOrder :: Order BidOrder -> BaseOrder
unBidOrder (Order order) = order
