{-# LANGUAGE DataKinds #-}

module Finance.Arbitrage.Types
  ( Spread(..)
  , SpreadBook
  , SpreadKey
  , SpreadMessage(..)
  ) where

import           Data.Map
import           Finance.Types

data Spread =
  Spread
    { spreadBid  :: !(Order BidOrder)
    , spreadAsks :: ![Order AskOrder]
    }
  deriving (Show)

data SpreadMessage
  = SpreadOpening Spread
  | SpreadUpdate Spread
  | SpreadClosing Spread
  deriving (Show)

type SpreadKey = String

type SpreadBook = Map SpreadKey Spread
