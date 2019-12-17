module Exchange.Kraken.Types (
       KrakenFeeTable(..)
) where

import qualified Data.Map as Map
import Finance.Types

data KrakenFeeTable = KrakenFeeTable {
     feesMap :: Map.Map CurrencyPair Double
} deriving (Show)

{- | AskOrders: The offered price gets multiplied by (1 + applicableFee)
     BidOrders: The offered price gets multiplied by (1 - applicableFee)
     If the Order for which we want the fee to be applied to is not an Order from Kraken, we will return  Nothing
-}
instance Fee KrakenFeeTable
    where applyFee (AskOrder (BaseOrder Kraken currency price qty timestamp)) feeTable
                | Just fee <- maybeFee = Just $ AskOrder (BaseOrder Kraken currency (price * (1 + fee)) qty timestamp)
                | Nothing <- maybeFee  = Nothing
                where maybeFee = Map.lookup currency (feesMap feeTable)
          applyFee (BidOrder (BaseOrder Kraken currency price qty timestamp)) feeTable
                | Just fee <- maybeFee = Just $ BidOrder (BaseOrder Kraken currency (price * (1 - fee)) qty timestamp)
                | Nothing <- maybeFee  = Nothing
                where maybeFee = Map.lookup currency (feesMap feeTable)
          applyFee _ _ = Nothing