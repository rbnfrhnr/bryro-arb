{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Kafka (
       KafkaConfig(..)
      ,createKafkaConfig
) where

import qualified Data.Configurator as Conf
import Control.Monad
import Data.ByteString
import Data.Configurator.Types
import System.IO

data KafkaConfig = KafkaConfig {
                   kafkaTopic :: !ByteString
                  ,kafkaHost  :: !ByteString
                  ,kafkaPort  :: !Integer
}

createKafkaConfig :: Config -> IO KafkaConfig
createKafkaConfig config = do
                            (Just topic) <- Conf.lookup config "kafka.topic"
                            (Just host) <- Conf.lookup config "kafka.host"
                            (Just port) <- Conf.lookup config "kafka.port"
                            return (KafkaConfig topic host port)