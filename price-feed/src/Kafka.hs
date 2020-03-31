{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Kafka (
       KafkaConfig(..)
      ,KafkaData
      ,createKafkaConfig
      ,getConnection
      ,toKafkaData
      ,writeToKafka
) where

import qualified Data.Configurator as Conf
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.ByteString
import Data.Configurator.Types
import System.IO
import Network.Kafka
import Network.Kafka.Consumer
import Network.Kafka.Producer
import Network.Kafka.Protocol
import Data.Int
import Data.Monoid
import Data.Semigroup

{- todo would it make sense to replace ByteString by their parsed Kafka types? -}
data KafkaConfig = KafkaConfig {
     kafkaTopic :: !ByteString
    ,kafkaHost  :: !ByteString
    ,kafkaPort  :: !Integer
    ,kafkaClientId :: !ByteString
}

data KafkaConnection = KafkaConnection {
     kafkaBatch  :: ![ByteString]
    ,kafkaState  :: (StateT KafkaState (ExceptT KafkaClientError IO) [ProduceResponse]) -> IO (Either KafkaClientError [ProduceResponse])
    ,kafkaConfig :: !KafkaConfig
}

createKafkaConfig :: Config -> IO KafkaConfig
createKafkaConfig config = do
                            (Just topic) <- Conf.lookup config "kafka.topic"
                            (Just host) <- Conf.lookup config "kafka.host"
                            (Just port) <- Conf.lookup config "kafka.port"
                            (Just clientId) <- Conf.lookup config "kafka.clientId"
                            return (KafkaConfig topic host port clientId)

configToConnection :: KafkaConfig -> IO KafkaConnection
configToConnection cfg@(KafkaConfig topic host port cid) = return $ KafkaConnection [] kafkaState cfg
                                                         where kafkaHost = Host $ KString $ host
                                                               kafkaPort = Port ((fromIntegral port) :: Int32)
                                                               kafkaState = (runKafka $ mkKafkaState (KString cid) (kafkaHost, kafkaPort))

getConnection :: Config -> IO KafkaConnection
getConnection cfg = createKafkaConfig cfg >>= configToConnection

{- todo exception handling -}
{- todo currently one connection -> one topic. Change it? -}
writeToKafka :: (KafkaData a) => KafkaConnection -> a -> IO KafkaConnection
writeToKafka (KafkaConnection batch kafkaState cfg) rawData = (kafkaState . produceMessages) [TopicAndMessage topicName (toKafkaData rawData)]
                                                              >> return (KafkaConnection [] kafkaState cfg)
                                                            where topicName = TName $ KString $ kafkaTopic cfg

instance Semigroup KafkaConnection where
         (<>) (KafkaConnection batch kafkaState cfg) (KafkaConnection batch2 _ _) = KafkaConnection (batch ++ batch2) kafkaState cfg

instance Monoid KafkaConnection where
         mempty = KafkaConnection []  kafkaState (KafkaConfig "" "" 9092 "")
                  where kafkaHost = Host $ KString $ ""
                        kafkaPort = Port ((fromIntegral 9092) :: Int32)
                        kafkaState = (runKafka $ mkKafkaState (KString "") (kafkaHost, kafkaPort))


class KafkaData a where
    toKafkaData :: a -> Message