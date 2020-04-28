{-# LANGUAGE FlexibleInstances #-}

module Ticker.Utils
  ( CurrencyExchangeKey
  , DBookMap
  , TickBuffer
  , printTickFiltered
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

printTickFiltered :: Maybe CurrencyExchangeKey -> (Maybe Order, Maybe Order) -> IO ()
printTickFiltered Nothing tick = printTick tick
printTickFiltered (Just currExchangeKey) (Just ask, Just bid)
  | currExchangeKey == toCurrencyExchangeKey ask = printTick (Just ask, Just bid)
  | otherwise = return ()
printTickFiltered _ _ = return ()

printTick :: (Maybe Order, Maybe Order) -> IO ()
printTick (Just ask, Just bid) =
  putStrLn
    (show (getExchangeFromOrder ask) ++
     " -> " ++
     show (getCurrencyPair ask) ++ " - " ++ show (getPriceFromOrder ask) ++ " / " ++ show (getPriceFromOrder bid)) >>
  hFlush stdout
printTick _ = return ()
