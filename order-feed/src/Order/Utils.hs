{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Order.Utils
  ( OrderFeedHandle(..)
  ) where

import qualified Control.Concurrent.Chan  as Chan
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as Map
import qualified Database.InfluxDB.Format as F

import           Control.Monad
import           Data.Aeson
import           Data.Time                (UTCTime)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           Database.InfluxDB
import           Database.InfluxDB.Line
import           Finance
import           Network.Kafka.Producer
import           Utils.Forward
import           Utils.Influx             as Influx
import           Utils.Kafka              as Kafka

data OrderFeedHandle =
  OrderFeedHandle
    { queue        :: Chan.Chan [BaseOrder]
    , destinations :: [Destination [BaseOrder]]
    }

instance InfluxData BaseOrder where
  toInfluxData (BaseOrder exchange currency price quantity time orderType) =
    (tags, fields, Just utcTime :: Maybe UTCTime)
    where
      tags =
        Map.fromList
          [ ("currency", stringFormatter (show currency))
          , ("exchange", stringFormatter (show exchange))
          , ("type", stringFormatter (show orderType))
          ]
      fields = Map.fromList [("price", FieldFloat price), ("quantity", FieldFloat quantity)]
      utcTime = posixSecondsToUTCTime $ realToFrac $ fromIntegral time / (1000 * 1000)
      stringFormatter = formatKey F.string

instance KafkaData BaseOrder where
  toKafkaData order = makeMessage $ BL.toStrict $ encode order

instance WriteOutIO Kafka.WriteHandle [BaseOrder] where
  writeOutIO wKafka orders = foldM Kafka.writeToKafka wKafka orders

instance WriteOutIO Influx.InfluxHandle [BaseOrder] where
  writeOutIO influxCon orders = foldM Influx.writeAsync influxCon orders
