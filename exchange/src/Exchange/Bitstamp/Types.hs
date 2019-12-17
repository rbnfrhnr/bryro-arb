{-# LANGUAGE OverloadedStrings #-}
module Exchange.Bitstamp.Types (
       BitstampFeeTable(..)
) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Finance.Types


data BitstampFeeTable = BitstampFeeTable {
     feesMap :: Map.Map CurrencyPair Double
} deriving (Show)

instance Aeson.FromJSON BitstampFeeTable where
    parseJSON (Aeson.Object v) = do
                        bchUsdFee         <- v Aeson..:? "bchusd_fee" Aeson..!= "0.0"
                        ethUsdFee         <- v Aeson..:? "ethusd_fee" Aeson..!= "0.0"
                        ltcUsdFee         <- v Aeson..:? "ltcusd_fee" Aeson..!= "0.0"
                        xrpUsdFee         <- v Aeson..:? "xrpusd_fee" Aeson..!= "0.0"
                        return $ BitstampFeeTable {
                                feesMap = ( Map.insert BCHUSD (read bchUsdFee :: Double)
                                          $ Map.insert ETHUSD (read ethUsdFee :: Double)
                                          $ Map.insert XRPUSD (read xrpUsdFee :: Double)
                                          $ Map.insert LTCUSD (read ltcUsdFee :: Double) (Map.empty))
                        }

{- | AskOrders: The offered price gets multiplied by (1 + applicableFee)
     BidOrders: The offered price gets multiplied by (1 - applicableFee)
     If the Order for which we want the fee to be applied to is not an Order from Bitstamp, we will return Nothing
-}
instance Fee BitstampFeeTable
    where applyFee (AskOrder (BaseOrder Bitstamp currency price qty timestamp)) feeTable
                | Just fee <- maybeFee = Just $ AskOrder (BaseOrder Bitstamp currency (price * (1 + fee)) qty timestamp)
                | Nothing <- maybeFee  = Nothing
                where maybeFee = Map.lookup currency (feesMap feeTable)
          applyFee (BidOrder (BaseOrder Bitstamp currency price qty timestamp)) feeTable
                | Just fee <- maybeFee = Just $ BidOrder (BaseOrder Bitstamp currency (price * (1 - fee)) qty timestamp)
                | Nothing <- maybeFee  = Nothing
                where maybeFee = Map.lookup currency (feesMap feeTable)
          applyFee _ _ = Nothing
