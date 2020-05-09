module Utils.Kafka.Internal
  ( KafkaData
  , addToBatch
  , toKafkaData
  ) where

import           Network.Kafka
import           Network.Kafka.Protocol

class KafkaData a where
  toKafkaData :: a -> Message

addToBatch :: (KafkaData a) => [TopicAndMessage] -> TopicName -> a -> [TopicAndMessage]
addToBatch batch topicName payload = batch ++ [TopicAndMessage topicName (toKafkaData payload)]
