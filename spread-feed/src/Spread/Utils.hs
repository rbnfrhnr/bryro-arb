{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Spread.Utils
  ( createDestinations
  ) where

import qualified Data.ByteString.Lazy    as BL

import           Control.Exception
import           Control.Monad           (foldM, liftM)
import           Data.Aeson
import           Data.Configurator
import           Data.Configurator.Types
import           Network.Kafka
import           Network.Kafka.Producer
import           System.FilePath
import           System.IO
import           Utils.Forward
import           Utils.Influx            as Influx
import           Utils.Kafka             as Kafka

createDestinations :: IO [Destination Int]
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
configFile = try $ load [Required $ "spread-feed" </> "resources" </> "config.cfg"]

createKafkaDest :: Config -> IO (Destination Int)
createKafkaDest cfg =
  Kafka.createKafkaConfig cfg >>= (\kafkaConfig -> pure (Destination (writeHandle kafkaConfig "bryro-spreads" 0)))

createInfluxDest :: Config -> IO (Destination Int)
createInfluxDest cfg = pure (Destination SimpleOut)

instance WriteOutIO WriteHandle Int where
  writeOutIO handle val = writeToKafka handle val

instance KafkaData Int where
  toKafkaData val = makeMessage $ BL.toStrict $ encode val
