{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Kafka
  ( KafkaConfig(..)
  , KafkaData
  , KafkaState
  , ReadHandle
  , WriteHandle
  , configToKafkaState
  , createKafkaConfig
  , readFromKafka
  , readHandle
  , toKafkaData
  , writeToKafka
  , writeHandle
  ) where

import qualified Data.Configurator       as Conf

import           Data.ByteString
import           Data.Configurator.Types
import           Network.Kafka
import           Network.Kafka.Consumer  (fetch', fetchMessages, fetchRequest)
import           Network.Kafka.Producer  (produceMessages)
import           Network.Kafka.Protocol
import           Utils.Kafka.Internal

{- | lookup the config values from a given config file -}
createKafkaConfig :: Config -> IO KafkaConfig
createKafkaConfig config = do
  (Just host) <- Conf.lookup config "kafka.host"
  (Just port) <- Conf.lookup config "kafka.port"
  (Just clientId) <- Conf.lookup config "kafka.clientId"
  return (KafkaConfig host port clientId)

{- | create ReadKafka, an enhanced datatype for reading from a specific topic and partition with a provided offset
     Every read action can be performed with such an instance
 -}
readHandle :: KafkaConfig -> TopicNameBS -> PartitionInt -> OffsetInt -> ConsumerGroupName -> ReadHandle
readHandle config topic parti offset groupName =
  ReadHandle
    (configToKafkaState config)
    (Offset (fromIntegral offset))
    (Partition (fromIntegral parti))
    (TName (KString topic))
    (ConsumerGroup (KString groupName))

{- | create WriteKafka. Additional attributes for writing to an already created kafkaState.
   Setting batchSize to 0 allows for direct writing to the kafka cluster.
-}
writeHandle :: KafkaConfig -> TopicNameBS -> BatchSize -> WriteHandle
writeHandle config topic batchSize = WriteHandle (configToKafkaState config) (TName (KString topic)) [] batchSize

{- | returns a WriteKafka instance since the buffer will be updated -}
writeToKafka :: (KafkaData a) => WriteHandle -> a -> IO WriteHandle
writeToKafka (WriteHandle kafkaState topicName batch batchSize) rawData
  | Prelude.length batch >= (batchSize - 1) =
    runKafka kafkaState (produceMessages updatedBatch) >>=
    either (\err -> print err >> pure (updatedHandle batch)) (\_ -> pure (updatedHandle []))
  | otherwise = return (WriteHandle kafkaState topicName updatedBatch batchSize)
  where
    updatedBatch = updateBatch batch topicName rawData
    updatedHandle batch' = WriteHandle kafkaState topicName batch' batchSize

{- | uses ReadKafka to read from a certain topic and returns the payload as bytesString if successful-}
readFromKafka :: ReadHandle -> IO (Either KafkaClientError [ByteString])
readFromKafka (ReadHandle state offsets parti topicName consumerGroup) =
  fmap (fmap (Prelude.map tamPayload . fetchMessages)) (runKafka state writeAndCommit)
  where
    writeAndCommit = do
      offset <- getLastOffset LatestTime parti topicName
      commitResp <- commitOffset (commitOffsetRequest consumerGroup topicName parti offset)
      withAnyHandle (\handle -> fetch' handle =<< fetchRequest offset parti topicName)
