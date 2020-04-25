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

instance KafkaData Tick where
  toKafkaData tick = makeMessage $ BL.toStrict $ encode tick

type CurrencyExchangeKey = String

type DBookMap = Map.Map CurrencyExchangeKey DepthBook

toCurrencyExchangeKey :: Order -> String
toCurrencyExchangeKey order = show (getCurrencyPair order) ++ show (getExchangeFromOrder order)

configFileKafka :: IO (Either SomeException Config)
configFileKafka = try $ load [Required $ "ticker-feed" </> "resources" </> "config.cfg"]

main :: IO ()
main = configFileKafka >>= withConfig

withConfig :: Either SomeException Config -> IO ()
withConfig (Right cfg) = do
  kafkaState <- liftM Kafka.configToKafkaState (Kafka.createKafkaConfig cfg)
  runTransform (readKafkaState kafkaState "bryro-orders" 0 0) (writeKafkaState kafkaState "bryro-ticker" 0) Map.empty

runTransform :: ReadKafka -> WriteKafka -> DBookMap -> IO ()
runTransform rKafka wKafka dBookMap =
  Kafka.readFromKafka rKafka >>= decodeOrders >>= return . foldl handleDepthBook dBookMap >>= runTransform rKafka wKafka >>
  return ()

handleDepthBook :: DBookMap -> Order -> DBookMap
handleDepthBook dBookMap order =
  case Map.lookup currencyExchangeKey dBookMap of
    Just depthBook -> Map.insert currencyExchangeKey (updateDepthBook depthBook order) dBookMap
    Nothing -> handleDepthBook (Map.insert currencyExchangeKey (openDepthBook (getCurrencyPair order)) dBookMap) order
  where
    currencyExchangeKey = toCurrencyExchangeKey order

decodeOrders :: Either KafkaClientError [BS.ByteString] -> IO [Order]
decodeOrders (Right msg) = return (foldl filteredDecode [] msg) :: IO [Order]
decodeOrders (Left err)  = fail "vla"

filteredDecode :: [Order] -> BS.ByteString -> [Order]
filteredDecode orders bsMessage =
  case decode (BL.fromStrict bsMessage) :: Maybe [Order] of
    (Just order) -> order ++ orders
    Nothing      -> orders
