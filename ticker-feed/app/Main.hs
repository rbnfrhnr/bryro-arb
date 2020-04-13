{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
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
import Finance.Depthbook.Types
import Finance.Depthbook.Utils


instance KafkaData Tick where
    toKafkaData tick = makeMessage $ BL.toStrict $ encode tick

type TickerST = Map.Map CurrencyPair (Map.Map Exchange (DepthBook, Tick))

configFileKafka :: IO (Either SomeException Config)
configFileKafka = try $ load [Required $ "resources" </> "config.cfg"]

main :: IO ()
main = configFileKafka >>= withConfig

withConfig :: Either SomeException Config -> IO ()
withConfig (Right cfg) = do
                         kafkaState <- liftM Kafka.configToKafkaState (Kafka.createKafkaConfig cfg)
                         influxConn <- Influx.getConnection cfg
                         runTransform (readKafkaState kafkaState "bryro-orders" 0 0) (writeKafkaState kafkaState "bryro-ticker" 0) Map.empty

runTransform :: ReadKafka -> WriteKafka -> TickerST -> IO ()
runTransform rKafka wKafka tickerST = Kafka.readFromKafka rKafka
                                            >>= decodeOrders
                                            >>= (\orders -> foldM (\ st order -> updateDepthBooks wKafka order st) tickerST orders)
                                            >>= (runTransform rKafka wKafka)
                                            >> return ()

updateDepthBooks :: WriteKafka -> Order -> TickerST -> IO TickerST
updateDepthBooks wKafka order tickerST = case (exchanges, maybeUpdatedBook, maybeTick) of
                                    ((Just excBook),(Just book), (Just tick)) -> (updateTick2 wKafka (Map.lookupMin (depthBookAsk book)) (Map.lookupMax (depthBookBid book)) tick) >>= (\ uTick -> return (book,uTick)) >>= (\ tpl -> return (Map.insert currency (Map.insert exchange tpl excBook) tickerST))
                                    ((Just excBook), Nothing, Nothing) -> updateDepthBooks wKafka order (Map.insert currency (Map.insert exchange ((openDepthBook currency), (Tick Nothing Nothing 0)) excBook) tickerST)
                                    (Nothing, _, _) -> updateDepthBooks wKafka order (Map.insert currency (Map.empty) tickerST)
            where currency = getCurrencyFromOrder order
                  exchange = getExchangeFromOrder order
                  exchanges = Map.lookup currency tickerST
                  maybeDBook = liftM fst (exchanges >>= (Map.lookup exchange))
                  maybeTick = liftM snd (exchanges >>= (Map.lookup exchange))
                  maybeUpdatedBook = fmap (\ book -> updateDepthBook book order) maybeDBook

updateTick2 :: WriteKafka -> Maybe (Order,Order) -> Maybe (Order, Order) -> Tick -> IO Tick
updateTick2 wKafka (Just (_,minAsk)) (Just (_,maxBid)) tick@(Tick (Just askTick) (Just bidTick) ts)
                                            | minAsk /= askTick && maxBid /= bidTick = Kafka.writeToKafka (\ _ -> return ()) wKafka updatedAskBid >> return (updatedAskBid)
                                            | minAsk /= askTick                     = Kafka.writeToKafka (\ _ -> return ()) wKafka updatedAsk >> return (updatedAsk)
                                            | maxBid /= bidTick                     = Kafka.writeToKafka (\ _ -> return ()) wKafka updatedBid >> return (updatedBid)
                                            | otherwise                            = return (tick)
                                            where updatedAskBid = updateTick maxBid (updateTick minAsk tick)
                                                  updatedAsk = updateTick minAsk tick
                                                  updatedBid = updateTick maxBid tick
updateTick2 wKafka Nothing (Just (_,maxBid)) tick@(Tick _ (Just bidTick) ts)
                                            | maxBid /= bidTick = Kafka.writeToKafka (\ _ -> return ()) wKafka updatedBid >> return (updatedBid)
                                            | otherwise = return (tick)
                                            where updatedBid = updateTick maxBid tick
updateTick2 wKafka (Just (_,minAsk)) Nothing tick@(Tick (Just askTick) _ ts)
                                            | minAsk /= askTick = Kafka.writeToKafka (\ _ -> return ()) wKafka updatedAsk >> return (updatedAsk)
                                            | otherwise = return (tick)
                                            where updatedAsk = updateTick minAsk tick
updateTick2 wKafka (Just (_,minAsk)) _ tick@(Tick Nothing _ ts) = Kafka.writeToKafka (\ _ -> return ()) wKafka updatedAsk >> return (updatedAsk)
                                            where updatedAsk = updateTick minAsk tick
updateTick2 wKafka _ (Just (_,maxBid)) tick@(Tick _ Nothing ts) = Kafka.writeToKafka (\ _ -> return ()) wKafka updatedBid >> return (updatedBid)
                                            where updatedBid = updateTick maxBid tick
updateTick2 wKafka a1@(_) a2@(_) tick@(_) = putStrLn ((show a1) ++ "\n" ++ (show a2) ++ "\n" ++ (show tick) ++ "\n\n\n") >> return (tick)


getExchangeFromOrder :: Order -> Exchange
getExchangeFromOrder (AskOrder order) = orderExchange order
getExchangeFromOrder (BidOrder order) = orderExchange order

getCurrencyFromOrder :: Order -> CurrencyPair
getCurrencyFromOrder (AskOrder order) = orderCurrencyPair order
getCurrencyFromOrder (BidOrder order) = orderCurrencyPair order

decodeOrders :: Either KafkaClientError [BS.ByteString] -> IO ([Order])
decodeOrders (Right msg) = return (foldl filteredDecode [] msg) :: IO ([Order])
decodeOrders (Left err) = fail "vla"

filteredDecode :: [Order] -> BS.ByteString -> [Order]
filteredDecode orders bsMessage = case (decode (BL.fromStrict bsMessage)) of
                                    (Just order) -> order : orders
                                    Nothing      -> orders
