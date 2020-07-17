{-# LANGUAGE DeriveGeneric #-}

module Finance.Currency
  ( CurrencyPair(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

{- | All the Supported currencies-}
data CurrencyPair
  = LTCUSD
  | XRPUSD
  | ETHUSD
  | BCHUSD
  deriving (Show, Generic)

instance ToJSON CurrencyPair

instance FromJSON CurrencyPair

-- | CurrencyPairs equality is lexicographic
instance Eq CurrencyPair where
  (==) cur1 cur2 = (==) (show cur1) (show cur2)

-- | CurrencyPairs ordering is lexicographic
instance Ord CurrencyPair where
  compare cur1 cur2 = compare (show cur1) (show cur2)
