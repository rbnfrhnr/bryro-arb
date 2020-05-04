module Finance.OrderBook.Types
  ( OrderBook(..)
  , OrderBookCollection(..)
  ) where

import           Data.Map
import           Finance.Types

{- | This Data-Structure keeps track of all the Ask and Bid prices of a certain Currency Pair.
     It holds prices regardless of their source exchange. It's ordered by Currency Pair.
     A valid CurrencyPair could be LTCUSD
-}
data OrderBook =
  OrderBook
    { depthBookCurrencyPair :: !CurrencyPair -- ^ Identifier for this Depthbook. (LTCUSD, XRPUSD etc)
    , depthBookAsk          :: Map Order Order -- ^ Collection of asking prices for this CurrencyPair
    , depthBookBid          :: Map Order Order -- ^ Collection of biding prices fot this CurrencyPair
    }
  deriving (Show)

-- | A collection of various DepthBooks. Identified by their CurrencyPair
type OrderBookCollection = Map CurrencyPair OrderBook
