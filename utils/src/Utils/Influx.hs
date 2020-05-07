{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Influx
  ( InfluxConfig(..)
  , InfluxConnection(..)
  , InfluxData
  , createInfluxConfig
  , getConnection
  , toInfluxData
  , writeToInflux
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

data InfluxConfig =
  InfluxConfig
    { influxHost        :: !T.Text
    , influxUser        :: !T.Text
    , influxPassword    :: !T.Text
    , influxMeasurement :: !String
    , influxDb          :: !String
    , influxBatchSize   :: !Int
    }

data InfluxConnection =
  InfluxConnection
    { influxBatch            :: ![Influx.Line UTCTime]
    , influxConnectionParams :: !Influx.WriteParams
    , influxConfig           :: !InfluxConfig
    }

class InfluxData a where
  toInfluxData :: a -> (Map.Map Influx.Key Influx.Key, Map.Map Influx.Key Influx.LineField, Maybe UTCTime)

createInfluxConfig :: Config -> IO InfluxConfig
createInfluxConfig config = do
  (Just host) <- Conf.lookup config "influx.host"
  (Just user) <- Conf.lookup config "influx.user"
  (Just password) <- Conf.lookup config "influx.password"
  (Just measurement) <- Conf.lookup config "influx.measurement"
  (Just db) <- Conf.lookup config "influx.db"
  (Just batchSize) <- Conf.lookup config "influx.batchSize"
  return (InfluxConfig host user password measurement db batchSize)

getConnectionConf :: InfluxConfig -> IO InfluxConnection
getConnectionConf cfg =
  return $
  InfluxConnection
    []
    (Influx.writeParams db & Influx.authentication ?~ cred & Influx.server . Influx.host .~ (influxHost cfg) &
     Influx.precision .~
     Influx.Nanosecond)
    cfg
  where
    db = Influx.formatDatabase F.string (influxDb cfg)
    cred = Influx.credentials (influxUser cfg) (influxPassword cfg)

getConnection :: Config -> IO InfluxConnection
getConnection cfgFile = createInfluxConfig cfgFile >>= getConnectionConf

writeToInflux :: (Show a, InfluxData a) => InfluxConnection -> a -> IO InfluxConnection
writeToInflux (InfluxConnection batch params cfg) rawData
  | length batch > batchSize =
    forkIO (Influx.writeBatch params (influxDataToLine measurement (toInfluxData rawData) : batch)) >>
    return (InfluxConnection [] params cfg)
  | otherwise = return (InfluxConnection (influxDataToLine measurement (toInfluxData rawData) : batch) params cfg)
  where
    batchSize = influxBatchSize cfg
    measurement = influxMeasurement cfg

influxDataToLine ::
     String
  -> (Map.Map Influx.Key Influx.Key, Map.Map Influx.Key Influx.LineField, Maybe UTCTime)
  -> Influx.Line UTCTime
influxDataToLine meas (tags, fields, timestamp) = Influx.Line (measurement meas) tags fields timestamp
  where
    measurement = F.formatMeasurement F.string
