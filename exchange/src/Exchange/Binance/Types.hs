{-# LANGUAGE OverloadedStrings #-}
module Exchange.Binance.Types (
       BinanceFeeTable(..)
) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Finance.Types

data BinanceFeeTable = BinanceFeeTable {
     feesMap :: Map.Map CurrencyPair Double
} deriving Show


instance Aeson.FromJSON BinanceFeeTable where
    parseJSON (Aeson.Object v) = do
                        takerCommission         <- v Aeson..:? "takerCommission" Aeson..!= 0.1
                        return $ BinanceFeeTable {
                                      feesMap = ( Map.insert BCHUSD (takerCommission)
                                                $ Map.insert ETHUSD (takerCommission)
                                                $ Map.insert XRPUSD (takerCommission)
                                                $ Map.insert LTCUSD (takerCommission) (Map.empty))
                                 }

{- | AskOrders: The offered price gets multiplied by (1 + applicableFee)
     BidOrders: The offered price gets multiplied by (1 - applicableFee)
     If the Order for which we want the fee to be applied to is not an Order from Binance, we will return Nothing
-}
instance Fee BinanceFeeTable
  where applyFee (AskOrder (BaseOrder Binance currency price qty timestamp)) feeTable
                 | Just fee <- maybeFee = Just $ AskOrder (BaseOrder Binance currency (price * (1 + fee)) qty timestamp)
                 | Nothing <- maybeFee  = Nothing
                 where maybeFee = Map.lookup currency (feesMap feeTable)
        applyFee (BidOrder (BaseOrder Binance currency price qty timestamp)) feeTable
                 | Just fee <- maybeFee = Just $ BidOrder (BaseOrder Binance currency (price * (1 - fee)) qty timestamp)
                 | Nothing <- maybeFee  = Nothing
                 where maybeFee = Map.lookup currency (feesMap feeTable)
        applyFee _ _ = Nothing


