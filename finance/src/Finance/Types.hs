{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Finance.Types
  ( BaseOrder(..)
  , CurrencyPair(..)
  , Exchange(..)
  , Order(..)
  , OrderPrice
  , OrderQty
  , Tick(..)
  , UpdateTick
  , updateTick
  ) where

import           Data.Aeson
import           Data.Map
import           GHC.Generics

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

{- | All the Supported currencies-}
data CurrencyPair
  = LTCUSD
  | XRPUSD
  | ETHUSD
  | BCHUSD
  deriving (Show, Generic)

{- | All the supported exchanges-}
data Exchange
  = Bitstamp
  | Kraken
  | Binance
  deriving (Show, Generic)

{- | Representation of a tick. e.g current lowest ask and highest bid orders -}
data Tick =
  Tick
    { timestamp :: !Int
    , tickAsk   :: !(Maybe Order)
    , tickBid   :: !(Maybe Order)
    }
  deriving (Show, Generic)

instance ToJSON BaseOrder

instance FromJSON BaseOrder

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

instance ToJSON Order

instance FromJSON Order

instance ToJSON CurrencyPair

instance FromJSON CurrencyPair

-- | CurrencyPairs equality is lexicographic
instance Eq CurrencyPair where
  (==) cur1 cur2 = (==) (show cur1) (show cur2)

-- | CurrencyPairs ordering is lexicographic
instance Ord CurrencyPair where
  compare cur1 cur2 = compare (show cur1) (show cur2)

instance ToJSON Exchange

instance FromJSON Exchange

instance Eq Exchange where
  (==) exch1 exch2 = (==) (show exch1) (show exch2)

instance Ord Exchange where
  compare exch1 exch2 = compare (show exch1) (show exch2)

class UpdateTick a where
  updateTick :: a -> Tick -> Tick

instance FromJSON Tick

instance ToJSON Tick
