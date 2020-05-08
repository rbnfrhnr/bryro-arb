{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Influx
  ( Fields
  , InfluxConfig
  , InfluxHandle
  , InfluxData
  , Keys
  , parseConfig
  , new
  , toInfluxData
  , writeAsync
  ) where

import qualified Data.Configurator        as Conf
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Database.InfluxDB        as Influx
import qualified Database.InfluxDB.Format as F

import           Control.Concurrent
import           Control.Lens
import           Data.Configurator.Types
import           Data.Time.Clock
import           Database.InfluxDB        (formatKey)
import           Utils.Influx.Internal

class InfluxData a where
  toInfluxData :: a -> (Keys, Fields, Maybe UTCTime)

parseConfig :: Config -> IO InfluxConfig
parseConfig config = do
  (Just host) <- Conf.lookup config "influx.host"
  (Just user) <- Conf.lookup config "influx.user"
  (Just password) <- Conf.lookup config "influx.password"
  (Just measurement) <- Conf.lookup config "influx.measurement"
  (Just db) <- Conf.lookup config "influx.db"
  (Just batchSize) <- Conf.lookup config "influx.batchSize"
  return (InfluxConfig host user password measurement db batchSize)

newFromConf :: InfluxConfig -> IO InfluxHandle
newFromConf cfg =
  return $
  InfluxHandle
    []
    (Influx.writeParams db & Influx.authentication ?~ cred & Influx.server . Influx.host .~ influxHost cfg &
     Influx.precision .~
     Influx.Nanosecond)
    cfg
  where
    db = Influx.formatDatabase F.string (influxDb cfg)
    cred = Influx.credentials (influxUser cfg) (influxPassword cfg)

new :: Config -> IO InfluxHandle
new cfgFile = parseConfig cfgFile >>= newFromConf

writeAsync :: (Show a, InfluxData a) => InfluxHandle -> a -> IO InfluxHandle
writeAsync (InfluxHandle batch params cfg) rawData
  | length batch > batchSize = forkIO (Influx.writeBatch params updatedBatch) >> return (InfluxHandle [] params cfg)
  | otherwise = return (InfluxHandle updatedBatch params cfg)
  where
    batchSize = influxBatchSize cfg
    measurement = influxMeasurement cfg
    dataLine = toDataLine measurement (toInfluxData rawData)
    updatedBatch = dataLine : batch
