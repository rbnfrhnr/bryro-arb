{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map                as Map

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Configurator
import           Data.Configurator.Types
import           Finance.Depthbook.Types
import           Finance.Depthbook.Utils
import           Finance.Types
import           Network.Kafka
import           Network.Kafka.Producer
import           System.FilePath
import           System.IO
import           Utils.Influx            as Influx
import           Utils.Kafka             as Kafka

instance KafkaData (Maybe Order, Maybe Order) where
  toKafkaData tick = makeMessage $ BL.toStrict $ encode tick

type CurrencyExchangeKey = String

type DBookMap = Map.Map CurrencyExchangeKey DepthBook

type TickBuffer = Map.Map CurrencyExchangeKey (Maybe Order, Maybe Order)

toCurrencyExchangeKey :: Order -> String
toCurrencyExchangeKey order = show (getCurrencyPair order) ++ show (getExchangeFromOrder order)

configFileKafka :: IO (Either SomeException Config)
configFileKafka = try $ load [Required $ "ticker-feed" </> "resources" </> "config.cfg"]

main :: IO ()
main = configFileKafka >>= withConfig

withConfig :: Either SomeException Config -> IO ()
withConfig (Right cfg) = do
  kafkaState <- fmap Kafka.configToKafkaState (Kafka.createKafkaConfig cfg)
  runTransform
    (readKafkaState kafkaState "bryro-orders" 0 0)
    (writeKafkaState kafkaState "bryro-ticker" 0)
    Map.empty
    Map.empty

runTransform :: ReadKafka -> WriteKafka -> TickBuffer -> DBookMap -> IO ()
runTransform rKafka wKafka tickBuffer dBookMap =
  Kafka.readFromKafka rKafka >>= decodeOrders >>= return . foldl handleDepthBook dBookMap >>= printLowestAsk >>=
  runTransform rKafka wKafka tickBuffer >>
  return ()

--  Kafka.readFromKafka rKafka >>= decodeOrders >>= return . foldl handleDepthBook dBookMap >>= runTransform rKafka wKafka >>
handleDepthBook :: DBookMap -> Order -> DBookMap
handleDepthBook dBookMap order =
  case Map.lookup currencyExchangeKey dBookMap of
    Just depthBook -> Map.insert currencyExchangeKey (updateDepthBook depthBook order) dBookMap
    Nothing -> handleDepthBook (Map.insert currencyExchangeKey (openDepthBook (getCurrencyPair order)) dBookMap) order
  where
    currencyExchangeKey = toCurrencyExchangeKey order

bufferedWrite :: WriteKafka -> Maybe (Maybe Order, Maybe Order) -> DepthBook -> IO (Maybe Order, Maybe Order)
bufferedWrite wKafka (Just (lastAsk, lastBid)) book
  | (lastAsk, lastBid) /= tickPair = Kafka.writeToKafka (\_ -> return ()) wKafka tickPair >> return tickPair
  where
    minAsk = fmap snd (Map.lookupMin (depthBookAsk book))
    maxBid = fmap snd (Map.lookupMax (depthBookBid book))
    tickPair = (minAsk, maxBid)

printLowestAsk :: DBookMap -> IO DBookMap
printLowestAsk bookMap =
  Map.traverseWithKey
    (\key dbook ->
       print (fmap ((++) (key ++ "  ") . show . getPriceFromOrder . snd) (Map.lookupMin (depthBookAsk dbook))) >>
       hFlush stdout >>
       return dbook)
    bookMap

decodeOrders :: Either KafkaClientError [BS.ByteString] -> IO [Order]
decodeOrders (Right msg) = return (foldl filteredDecode [] msg) :: IO [Order]
decodeOrders (Left err)  = fail "vla"

filteredDecode :: [Order] -> BS.ByteString -> [Order]
filteredDecode orders bsMessage =
  case decode (BL.fromStrict bsMessage) :: Maybe [Order] of
    (Just order) -> order ++ orders
    Nothing      -> orders
