{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Kafka
  ( KafkaConfig(..)
  , KafkaData
  , KafkaState
  , ReadKafka(..)
  , WriteKafka(..)
  , configToKafkaState
  , createKafkaConfig
  , readFromKafka
  , readKafkaState
  , toKafkaData
  , writeToKafka
  , writeKafkaState
  ) where

import           Control.Monad
import           Data.ByteString
import qualified Data.Configurator       as Conf
import           Data.Configurator.Types
import           Network.Kafka
import           Network.Kafka.Consumer
import           Network.Kafka.Producer
import           Network.Kafka.Protocol

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

data WriteKafka =
  WriteKafka
    { wKafkaState     :: !KafkaState
    , wKafkaTopic     :: !TopicName
    , wKafkaBatch     :: ![TopicAndMessage]
    , wKafkaBatchSize :: !Int
    }
  deriving (Show)

data ReadKafka =
  ReadKafka
    { rKafkaState     :: !KafkaState
    , rKafkaOffset    :: !Offset
    , rKafkaPartition :: !Partition
    , rKafkaTopic     :: !TopicName
    }
  deriving (Show)

class KafkaData a where
  toKafkaData :: a -> Message

{- | lookup the config values from a given config file -}
createKafkaConfig :: Config -> IO KafkaConfig
createKafkaConfig config = do
  (Just host) <- Conf.lookup config "kafka.host"
  (Just port) <- Conf.lookup config "kafka.port"
  (Just clientId) <- Conf.lookup config "kafka.clientId"
  return (KafkaConfig host port clientId)

{- | convert the config properties (host port etc) to an actual KafkaState -}
configToKafkaState :: KafkaConfig -> KafkaState
configToKafkaState (KafkaConfig host port cid) =
  mkKafkaState (KString cid) (Host (KString host), Port (fromIntegral port))

{- | create ReadKafka, an enhanced datatype for reading from a specific topic and partition with a provided offset
     Every read action can be performed with such an instance
 -}
readKafkaState :: KafkaState -> ByteString -> Int -> Int -> ReadKafka
readKafkaState state topic parti offset =
  ReadKafka state (Offset (fromIntegral offset)) (Partition (fromIntegral parti)) (TName (KString topic))

{- | create WriteKafka. Additional attributes for writing to an already created kafkaState.
   Setting batchSize to 0 allows for direct writing to the kafka cluster.
-}
writeKafkaState :: KafkaState -> ByteString -> Int -> WriteKafka
writeKafkaState state topic batchSize = WriteKafka state (TName (KString topic)) [] batchSize

{- | returns a WriteKafka instance since the buffer will be updated -}
writeToKafka ::
     (KafkaData a) => (Either KafkaClientError [ProduceResponse] -> IO ()) -> WriteKafka -> a -> IO WriteKafka
writeToKafka respHandler (WriteKafka kafkaState topicName batch batchSize) rawData
  | Prelude.length batch >= (batchSize - 1) =
    runKafka kafkaState (produceMessages (addToBatch batch topicName rawData)) >>= respHandler >>
    return (WriteKafka kafkaState topicName [] batchSize)
  | otherwise = return (WriteKafka kafkaState topicName (addToBatch batch topicName rawData) batchSize)

{- | uses ReadKafka to read from a certain topic and returns the payload as bytesString if successful-}
readFromKafka :: ReadKafka -> IO (Either KafkaClientError [ByteString])
readFromKafka (ReadKafka state offsets parti topicName) =
  liftM
    (fmap (Prelude.map tamPayload . fetchMessages))
    (runKafka
       state
       (do offset <- getLastOffset LatestTime parti topicName
           commitResp <- commitOffset (commitOffsetRequest (ConsumerGroup "ticker-consumer") topicName parti offset)
           withAnyHandle (\handle -> fetch' handle =<< fetchRequest offset parti topicName)))

{- INTERNAL  (Move into Internal Module -}
addToBatch :: (KafkaData a) => [TopicAndMessage] -> TopicName -> a -> [TopicAndMessage]
addToBatch batch topicName payload = batch ++ [TopicAndMessage topicName (toKafkaData payload)]
