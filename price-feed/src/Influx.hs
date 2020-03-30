{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Influx (
       InfluxConfig(..)
      ,createInfluxConfig
      ,getConnectionConf
      ,writeToInflux
) where

import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Database.InfluxDB as Influx
import Control.Monad
import Data.Configurator.Types
import System.IO
import Data.Time.Clock
import Control.Lens
import Data.Int
import qualified Database.InfluxDB.Format as F
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
getConnectionConf cfg = do
                        let db = Influx.formatDatabase F.string (influxDb cfg)
                        let cred = Influx.credentials (influxUser cfg) (influxPassword cfg)
                        return $ InfluxConnection [] (Influx.writeParams db & Influx.authentication ?~ cred & Influx.server.Influx.host .~ (influxHost cfg) & Influx.precision .~ Influx.Nanosecond) cfg

getConnection :: Config -> IO InfluxConnection
getConnection cfgFile = createInfluxConfig cfgFile >>= getConnectionConf

writeToInflux :: InfluxConnection -> [Influx.Line UTCTime] -> IO InfluxConnection
writeToInflux (InfluxConnection batch params cfg) line
                                                       | length batch > batchSize = forkIO (Influx.writeBatch params (line ++ batch)) >> return (InfluxConnection [] params cfg)
                                                       | otherwise                = return (InfluxConnection (line ++ batch) params cfg)
                                                       where batchSize = influxBatchSize cfg