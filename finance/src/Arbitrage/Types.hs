module Arbitrage.Types (
       Spread(..)
      ,SpreadBook
      ,SpreadKey
      ,SpreadMessage(..)
) where


import Types
import Data.Map

data Spread = Spread {
     spreadBid  :: !Order
    ,spreadAsks :: ![Order]
} deriving (Show)

data SpreadMessage = SpreadOpening Spread
                   | SpreadUpdate Spread
                   | SpreadClosing Spread
                    deriving (Show)

type SpreadKey = String

type SpreadBook = Map SpreadKey Spread
