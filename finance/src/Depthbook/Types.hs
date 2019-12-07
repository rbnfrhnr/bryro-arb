module Depthbook.Types
       (
         DepthBook(..)
        ,DepthBookCollection(..)
       ) where

import Data.Map
import Types

{- | This Data-Structure keeps track of all the Ask and Bid prices of a certain Currency Pair.
     It holds prices regardless of their source exchange. It's ordered by Currency Pair.
     A valid CurrencyPair could be LTCUSD
-}
data DepthBook = DepthBook {
     depthBookCurrencyPair :: !CurrencyPair      -- ^ Identifier for this Depthbook. (LTCUSD, XRPUSD etc)
    ,depthBookAsk          :: Map Order Order -- ^ Collection of asking prices for this CurrencyPair
    ,depthBookBid          :: Map Order Order -- ^ Collection of biding prices fot this CurrencyPair
} deriving (Show)

-- | A collection of various DepthBooks. Identified by their CurrencyPair
type DepthBookCollection = Map CurrencyPair DepthBook
