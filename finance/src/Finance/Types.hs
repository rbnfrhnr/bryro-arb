module Finance.Types
    (
       BaseOrder(..)
      ,CurrencyPair(..)
      ,Exchange(..)
      ,Order(..)


      ,getCurrencyPair
    ) where

import Data.Map

{- | Basic typeclass to convert certain values to a csv representation -}
class Csv a where
    toCsv :: a -> String


{- | Base representation of an order offered through an exchange. -}
data BaseOrder = BaseOrder {
   orderExchange     :: !Exchange     -- ^ the exchange from which the order originated
  ,orderCurrencyPair :: !CurrencyPair -- ^ the currencypair for which the order is for
  ,orderCurrentPrice :: !Double       -- ^ the currently offered price for this order.
  ,orderQuantity     :: !Double       -- ^ the offered quantity for this order
  ,orderTimestamp    :: !Int          -- ^ the time at which this order was last updated / created
} deriving (Show)


{- | An order can either be an Ask- or a Bid-Order-}
data Order = AskOrder BaseOrder
            |BidOrder BaseOrder
            deriving (Show)


data CurrencyPair = LTCUSD | XRPUSD | ETHUSD | BCHUSD deriving (Show)

data Exchange = Bitstamp | Kraken | Binance deriving (Show)

-- | CurrencyPairs equality is lexicographic
instance Eq CurrencyPair where
    (==) cur1 cur2 = (==) (show cur1) (show cur2)

-- | CurrencyPairs ordering is lexicographic
instance Ord CurrencyPair where
    compare cur1 cur2 = compare (show cur1) (show cur2)


{- | A prices equality is defined solely bi its price offered at an exchange -}
instance Eq Order where
    (==) (AskOrder (BaseOrder _ _ price _ _)) (AskOrder (BaseOrder _ _ price2 _ _)) = (==) price price2
    (==) (AskOrder (BaseOrder _ _ price _ _)) (BidOrder (BaseOrder _ _ price2 _ _)) = (==) price price2
    (==) (BidOrder (BaseOrder _ _ price _ _)) (AskOrder (BaseOrder _ _ price2 _ _)) = (==) price price2
    (==) (BidOrder (BaseOrder _ _ price _ _)) (BidOrder (BaseOrder _ _ price2 _ _)) = (==) price price2

{- | Prices are ordered by their currently offered price at an exchange -}
instance Ord Order where
    compare (AskOrder (BaseOrder _ _ price _ _)) (AskOrder (BaseOrder _ _ price2 _ _)) = compare price price2
    compare (AskOrder (BaseOrder _ _ price _ _)) (BidOrder (BaseOrder _ _ price2 _ _)) = compare price price2
    compare (BidOrder (BaseOrder _ _ price _ _)) (AskOrder (BaseOrder _ _ price2 _ _)) = compare price price2
    compare (BidOrder (BaseOrder _ _ price _ _)) (BidOrder (BaseOrder _ _ price2 _ _)) = compare price price2

instance Csv Order where
    toCsv (AskOrder (BaseOrder exchange symbol price qty timestamp)) = "Ask," ++ show exchange ++ "," ++ show symbol ++ "," ++ show price ++ "," ++ show qty ++ "," ++ show timestamp
    toCsv (BidOrder (BaseOrder exchange symbol price qty timestamp)) = "Bid," ++ show exchange ++ "," ++ show symbol ++ "," ++ show price ++ "," ++ show qty ++ "," ++ show timestamp

type PriceConstructor = Exchange -> CurrencyPair -> Double -> Double -> Int -> Order

getCurrencyPair :: Order -> CurrencyPair
getCurrencyPair (AskOrder (BaseOrder _ pair _ _ _ )) = pair
getCurrencyPair (BidOrder (BaseOrder _ pair _ _ _ )) = pair
