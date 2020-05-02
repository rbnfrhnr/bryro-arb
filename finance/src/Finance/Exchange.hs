{-# LANGUAGE DeriveGeneric #-}

module Finance.Exchange
  ( Exchange(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

{- | All the supported exchanges-}
data Exchange
  = Bitstamp
  | Kraken
  | Binance
  deriving (Show, Generic)

instance ToJSON Exchange

instance FromJSON Exchange

instance Eq Exchange where
  (==) exch1 exch2 = (==) (show exch1) (show exch2)

instance Ord Exchange where
  compare exch1 exch2 = compare (show exch1) (show exch2)
