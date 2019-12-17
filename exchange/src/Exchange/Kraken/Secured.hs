module Exchange.Kraken.Secured (
       getKrakenFee
) where

import qualified Data.Map as Map
import Exchange.Kraken.Types

getKrakenFee :: IO (Maybe KrakenFeeTable)
getKrakenFee = return (Just (KrakenFeeTable (Map.empty)))