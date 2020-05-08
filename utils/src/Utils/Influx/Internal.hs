module Utils.Influx.Internal
  ( Fields
  , Keys
  , InfluxConfig(..)
  , InfluxHandle(..)
  , toDataLine
  ) where

import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Data.Time.Clock          as Clk
import qualified Database.InfluxDB        as Influx
import qualified Database.InfluxDB.Format as F

type Keys = (Map.Map Influx.Key Influx.Key)

type Fields = (Map.Map Influx.Key Influx.LineField)

data InfluxConfig =
  InfluxConfig
    { influxHost        :: !T.Text
    , influxUser        :: !T.Text
    , influxPassword    :: !T.Text
    , influxMeasurement :: !String
    , influxDb          :: !String
    , influxBatchSize   :: !Int
    }

data InfluxHandle =
  InfluxHandle
    { influxBatch            :: ![Influx.Line Clk.UTCTime]
    , influxConnectionParams :: !Influx.WriteParams
    , influxConfig           :: !InfluxConfig
    }

toDataLine :: String -> (Keys, Fields, Maybe Clk.UTCTime) -> Influx.Line Clk.UTCTime
toDataLine meas (tags, fields, timestamp) = Influx.Line (measurement meas) tags fields timestamp
  where
    measurement = F.formatMeasurement F.string
