module Utils.Kafka.Internal
  ( BatchSize
  , ConsumerGroupName
  , KafkaConfig(..)
  , KafkaData
  , KafkaState(..)
  , ReadHandle(..)
  , WriteHandle(..)
  , OffsetInt
  , PartitionInt
  , TopicNameBS
  , updateBatch
  , configToKafkaState
  , toKafkaData
  ) where

import           Data.ByteString
import           Network.Kafka
import           Network.Kafka.Protocol

type ConsumerGroupName = ByteString

type PartitionInt = Int

type OffsetInt = Int

type TopicNameBS = ByteString

type BatchSize = Int

data KafkaConfig =
  KafkaConfig
    { kafkaHost     :: !ByteString
    , kafkaPort     :: !Integer
    , kafkaClientId :: !ByteString
    }
  deriving (Show)

data KafkaConnection =
  KafkaConnection
    { kafkaState  :: !KafkaState
    , kafkaConfig :: !KafkaConfig
    }
  deriving (Show)

data WriteHandle =
  WriteHandle
    { wKafkaState     :: !KafkaState
    , wKafkaTopic     :: !TopicName
    , wKafkaBatch     :: ![TopicAndMessage]
    , wKafkaBatchSize :: !Int
    }
  deriving (Show)

data ReadHandle =
  ReadHandle
    { rKafkaState         :: !KafkaState
    , rKafkaOffset        :: !Offset
    , rKafkaPartition     :: !Partition
    , rKafkaTopic         :: !TopicName
    , rKafkaConsumerGroup :: ConsumerGroup
    }
  deriving (Show)

class KafkaData a where
  toKafkaData :: a -> Message

{- | convert the config properties (host port etc) to an actual KafkaState -}
configToKafkaState :: KafkaConfig -> KafkaState
configToKafkaState (KafkaConfig host port cid) =
  mkKafkaState (KString cid) (Host (KString host), Port (fromIntegral port))

updateBatch :: (KafkaData a) => [TopicAndMessage] -> TopicName -> a -> [TopicAndMessage]
updateBatch batch topicName payload = batch ++ [TopicAndMessage topicName (toKafkaData payload)]
