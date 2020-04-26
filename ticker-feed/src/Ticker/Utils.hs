{-# LANGUAGE FlexibleInstances #-}

module Ticker.Utils
  ( CurrencyExchangeKey
  , DBookMap
  , TickBuffer
  , printLowestAsk
  , toCurrencyExchangeKey
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map                as Map
import           Finance.Depthbook.Types
import           Finance.Types
import           Network.Kafka
import           Network.Kafka.Producer
import           System.IO
import           Utils.Kafka             as Kafka

instance KafkaData (Maybe Order, Maybe Order) where
  toKafkaData tick = makeMessage $ BL.toStrict $ encode tick

type CurrencyExchangeKey = String

type DBookMap = Map.Map CurrencyExchangeKey DepthBook

type TickBuffer = Map.Map CurrencyExchangeKey (Maybe Order, Maybe Order)

toCurrencyExchangeKey :: Order -> String
toCurrencyExchangeKey order = show (getCurrencyPair order) ++ show (getExchangeFromOrder order)

printLowestAsk :: DBookMap -> IO DBookMap
printLowestAsk bookMap =
  Map.traverseWithKey
    (\key dbook ->
       print (fmap ((++) (key ++ "  ") . show . getPriceFromOrder . snd) (Map.lookupMin (depthBookAsk dbook))) >>
       hFlush stdout >>
       return dbook)
    bookMap
