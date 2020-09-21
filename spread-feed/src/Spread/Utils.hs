{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Spread.Utils
  ( createDestinations
  ) where

import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Database.InfluxDB.Format as F

import           Control.Exception
import           Control.Monad            (foldM, liftM)
import           Data.Aeson
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Time                (UTCTime)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           Database.InfluxDB        (formatKey)
import           Database.InfluxDB.Line
import           Finance
import           Network.Kafka
import           Network.Kafka.Producer
import           System.FilePath
import           System.IO
import           Utils.Forward
import           Utils.Influx             as Influx
import           Utils.Kafka              as Kafka

createDestinations :: IO [Destination SpreadMessage]
createDestinations =
  configFile >>=
  either
    (\exc -> print exc >> pure [])
    (\cfg ->
       foldM
         (\dest cur -> cur >>= (\realCurl -> pure (dest ++ [realCurl])))
         []
         [createKafkaDest cfg, createInfluxDest cfg])

configFile :: IO (Either SomeException Config)
configFile = try $ load [Required $ "resources" </> "config.cfg"]

createKafkaDest :: Config -> IO (Destination SpreadMessage)
createKafkaDest cfg =
  Kafka.createKafkaConfig cfg >>= (\kafkaConfig -> pure (Destination (writeHandle kafkaConfig "bryro-spreads" 0)))

createInfluxDest :: Config -> IO (Destination SpreadMessage)
createInfluxDest cfg = Destination <$> new cfg

instance WriteOutIO InfluxHandle SpreadMessage where
  writeOutIO influxCon val = Influx.writeAsync influxCon val

instance WriteOutIO WriteHandle SpreadMessage where
  writeOutIO handle val = writeToKafka handle val

instance KafkaData SpreadMessage where
  toKafkaData val = makeMessage $ BL.toStrict $ encode val

instance InfluxData Spread where
  toInfluxData (Spread spreadKey bidOrder' askOrders) = (tags, fields, Nothing :: Maybe UTCTime)
    where
      tags =
        Map.fromList
          [ ("currency", stringFormatter (show (orderCurrencyPair bidOrder)))
          , ("buyFrom", stringFormatter (show (orderExchange bidOrder)))
          , ("spreadId", stringFormatter spreadKey)
          ]
      fields =
        Map.fromList
          [ ("bidPrice", FieldFloat (orderCurrentPrice bidOrder))
          , ("spreadId", FieldString (T.pack spreadKey))
          , ("askOrders", FieldString (TE.decodeUtf8 (BL.toStrict (encode askOrders))))
          ]
      stringFormatter = formatKey F.string
      bidOrder = unBidOrder bidOrder'

instance InfluxData SpreadMessage where
  toInfluxData (SpreadOpening spread) = addType "Opening" (toInfluxData spread)
  toInfluxData (SpreadUpdate spread) = addType "Update" (toInfluxData spread)
  toInfluxData (SpreadClosing spread) = addType "Closing" (toInfluxData spread)

addType :: String -> (Keys, Fields, Maybe UTCTime) -> (Keys, Fields, Maybe UTCTime)
addType msgType (tags, fields, time) = (Map.union tags typeTag, Map.union fields typeField, time)
  where
    typeTag = Map.fromList [("msgType", stringFormatter msgType)]
    typeField = Map.fromList [("msgType", FieldString (T.pack msgType))]
    stringFormatter = formatKey F.string
