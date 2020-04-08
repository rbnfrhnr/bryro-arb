{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Configurator
import Data.Configurator.Types
import System.FilePath
import Utils.Influx as Influx
import Utils.Kafka as Kafka
import Network.Kafka
import Network.Kafka.Producer
import System.IO
import Finance.Types


instance KafkaData Order where
    toKafkaData order = makeMessage $ BL.toStrict $ encode order

configFileKafka :: IO (Either SomeException Config)
configFileKafka = try $ load [Required $ "resources" </> "config.cfg"]

main :: IO ()
main = configFileKafka >>= withConfig


withConfig :: Either SomeException Config -> IO ()
withConfig (Right cfg) = do
                         kafkaState <- liftM Kafka.configToKafkaState (Kafka.createKafkaConfig cfg)
                         influxConn <- Influx.getConnection cfg
                         transformOrders (readKafkaState kafkaState "bryro-orders" 0 0) (writeKafkaState kafkaState "bryro-ticker" 0) influxConn

transformOrders :: ReadKafka -> WriteKafka -> InfluxConnection -> IO ()
transformOrders rKafka wKafka influxConn = (Kafka.readFromKafka rKafka)
                                           >>= decodeOrders
                                           >>= (\ eitherOrders -> writeEither eitherOrders wKafka)
                                           >> transformOrders rKafka wKafka influxConn

writeEither :: Either KafkaClientError [Maybe Order] -> WriteKafka -> IO WriteKafka
writeEither (Right maybeMsgs) wKafka = writeSingleOrder wKafka maybeMsgs

writeSingleOrder :: WriteKafka -> [Maybe Order] -> IO WriteKafka
writeSingleOrder wKafka ((Just order):xs) = Kafka.writeToKafka (\ err -> return ()) wKafka order >>= (\ wk -> writeSingleOrder wk xs)
writeSingleOrder wKafka ((Nothing):xs) = writeSingleOrder wKafka xs
writeSingleOrder wKafka (_) = return (wKafka)

decodeOrders :: Either KafkaClientError [BS.ByteString] -> IO (Either KafkaClientError ([Maybe Order]))
decodeOrders (Right msg) = return (Right (map (decode . BL.fromStrict) msg)) :: IO (Either KafkaClientError ([Maybe Order]))
decodeOrders (Left err) = return (Left err)

printl :: Either KafkaClientError ([Maybe Order]) -> IO ()
printl (Right msg) = putStrLn (show msg) >> (hFlush stdout) >> return ()
printl (Left err) = putStrLn (show err) >> (hFlush stdout) >> return ()