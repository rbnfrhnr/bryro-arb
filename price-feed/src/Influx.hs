{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Influx (
       InfluxConfig(..)
      ,createInfluxConfig
) where

import qualified Data.Configurator as Conf
import qualified Data.Text as T
import Control.Monad
import Data.Configurator.Types
import System.IO

data InfluxConfig = InfluxConfig {
     influxHost        :: !T.Text
    ,influxUser        :: !T.Text
    ,influxPassword    :: !T.Text
    ,influxMeasurement :: !String
    ,influxDb          :: !String
}

createInfluxConfig :: Config -> IO InfluxConfig
createInfluxConfig config = do
                            (Just host) <- Conf.lookup config "influx.host"
                            (Just user) <- Conf.lookup config "influx.user"
                            (Just password) <- Conf.lookup config "influx.password"
                            (Just measurement) <- Conf.lookup config "influx.measurement"
                            (Just db) <- Conf.lookup config "influx.db"
                            return (InfluxConfig host user password measurement db)