{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.Influx (
       InfluxConfig(..)
      ,InfluxConnection(..)
      ,InfluxData

      ,createInfluxConfig
      ,getConnection
      ,toInfluxLine
      ,writeToInflux
) where

import qualified Data.Configurator as Conf
import qualified Database.InfluxDB as Influx
import qualified Database.InfluxDB.Format as F
import qualified Data.Text as T
import Data.Configurator.Types
import Data.Time.Clock
import Control.Lens
import Control.Concurrent

data InfluxConfig = InfluxConfig {
     influxHost        :: !T.Text
    ,influxUser        :: !T.Text
    ,influxPassword    :: !T.Text
    ,influxMeasurement :: !String
    ,influxDb          :: !String
    ,influxBatchSize   :: !Int
}

data InfluxConnection = InfluxConnection {
    influxBatch            :: ![Influx.Line UTCTime]
   ,influxConnectionParams :: !Influx.WriteParams
   ,influxConfig           :: !InfluxConfig
}

class InfluxData a where
    toInfluxLine :: a -> Influx.Line UTCTime

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
getConnectionConf cfg = return $ InfluxConnection [] (Influx.writeParams db & Influx.authentication ?~ cred & Influx.server.Influx.host .~ (influxHost cfg) & Influx.precision .~ Influx.Nanosecond) cfg
                where db = Influx.formatDatabase F.string (influxDb cfg)
                      cred = Influx.credentials (influxUser cfg) (influxPassword cfg)

getConnection :: Config -> IO InfluxConnection
getConnection cfgFile = createInfluxConfig cfgFile >>= getConnectionConf

writeToInflux :: (Show a, InfluxData a) => InfluxConnection -> a -> IO InfluxConnection
writeToInflux (InfluxConnection batch params cfg) rawData
                                                       | length batch > batchSize = forkIO (Influx.writeBatch params ((toInfluxLine rawData) : batch)) >> return (InfluxConnection [] params cfg)
                                                       | otherwise                = return (InfluxConnection ((toInfluxLine rawData) : batch) params cfg)
                                                       where batchSize = influxBatchSize cfg
