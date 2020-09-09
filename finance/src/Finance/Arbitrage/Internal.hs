{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Finance.Arbitrage.Internal
  ( Spread(..)
  , SpreadBook
  , SpreadKey
  , SpreadMessage(..)
  ) where

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Map
import           Finance.Order
import           GHC.Generics  (Generic)

data Spread =
  Spread
    { spreadKey  :: SpreadKey
    , spreadBid  :: !(Order BidOrder)
    , spreadAsks :: ![Order AskOrder]
    }
  deriving (Show, Generic)

instance ToJSON Spread

data SpreadMessage
  = SpreadOpening Spread
  | SpreadUpdate Spread
  | SpreadClosing Spread
  deriving (Show, Generic)

instance ToJSON SpreadMessage

type SpreadKey = String

type SpreadBook = Map SpreadKey Spread
