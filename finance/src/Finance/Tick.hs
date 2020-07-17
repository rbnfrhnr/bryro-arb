{-# LANGUAGE DeriveGeneric #-}

module Finance.Tick
  ( Tick(..)
  ) where

import           Data.Aeson       (FromJSON, ToJSON)
import           Finance.Currency (CurrencyPair)
import           Finance.Exchange (Exchange)
import           Finance.Order
import           GHC.Generics     (Generic)

data Tick =
  Tick
    { tickAsk       :: !(Double, Double)
    , tickBid       :: !(Double, Double)
    , tickCurrency  :: !CurrencyPair
    , tickExchange  :: !Exchange
    , tickTimestamp :: !Int
    }
  deriving (Show, Generic)

instance ToJSON Tick

instance FromJSON Tick

instance Eq Tick where
  (==) (Tick ask1 bid1 curr1 exch1 _) (Tick ask2 bid2 curr2 exch2 _) =
    ask1 == ask2 && bid1 == bid2 && curr1 == curr2 && exch1 == exch2
