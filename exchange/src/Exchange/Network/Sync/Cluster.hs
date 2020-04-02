{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Exchange.Network.Sync.Cluster (
       test2
) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent
import Control.Monad
import Exchange.Network.Sync.Types
import Exchange.Network.Sync.Utils
import System.IO

import Exchange.Kraken.Utils as K
import Exchange.Bitstamp.Utils as BIT

data TransformerData a = TransformerData a deriving (Show)

{- | takes the TransformerData buffer a Chan and allows you to write to the output channel (not mandatory).
     returns a new TransformerData state
-}
class WriteOut a b where
    writeOut :: Chan b -> TransformerData a  -> IO (TransformerData a)

{- | Typeclass for transforming an incoming message. Acts as a buffer before writing to the output channel. -}
class UpdateTransformerState a t where
    updateTState :: TransformerData t -> a -> IO (TransformerData t)


{- | Pipes messages from a source to a sink and transforms the data in-between -}
filteredChan :: (WriteOut t o, UpdateTransformerState i t) => Chan.Chan i -> Chan.Chan o -> TransformerData t -> IO (TransformerData t)
filteredChan source sink tdState = (Chan.readChan source) >>= (updateTState tdState) >>= (writeOut sink) >>= (filteredChan source sink)

{- | Runs a filtered channel composite. Run it in a thread or otherwise it will block -}
runFilteredChan :: IO (TransformerData t) -> IO ()
runFilteredChan fChan = fChan >> runFilteredChan fChan


{- | Simple data structure for deduplication of messages in a Chan -}
data DeduplicationTState = DeduplicationTState (Maybe BL.ByteString) (Map.Map BL.ByteString BL.ByteString) deriving (Show)

instance WriteOut DeduplicationTState BL.ByteString where
    writeOut chan (TransformerData (DeduplicationTState (Just msg) lookupMap)) = Chan.writeChan chan msg >> return (TransformerData $ DeduplicationTState Nothing (Map.insert msg msg lookupMap))
    writeOut chan (TransformerData (DeduplicationTState _ lookupMap))          = return $ TransformerData $ DeduplicationTState Nothing lookupMap

instance UpdateTransformerState BL.ByteString DeduplicationTState where
    updateTState (TransformerData (DeduplicationTState _ lookupMap)) bs = case Map.lookup bs lookupMap of
                                                                   (Just _) -> return $ TransformerData $ DeduplicationTState Nothing lookupMap
                                                                   Nothing  -> return $ TransformerData $ DeduplicationTState (Just bs) lookupMap

pipeChan :: Chan.Chan a -> Chan.Chan a -> IO ()
pipeChan source sink = (Chan.readChan source) >>= (Chan.writeChan sink)

runPipedChan :: IO () -> IO ()
runPipedChan pipedChan = pipedChan >> runPipedChan pipedChan


--currentMsgInChan <- Chan.newChan :: IO (Chan BL.ByteString) -- current messages from websockets
--bufferChan <- Chan.newChan :: IO (Chan BL.ByteString) -- man in the middle
--outChan <- Chan.newChan :: IO (Chan BL.ByteString) -- propagates to cluster subscribers



liveReconnect :: SyncedWSCluster -> IO (SyncedWSCluster)
liveReconnect cluster@(SyncedWSCluster outChannel inChannel nodeMap oldChannelToOutTid) = do
    bufferChan <- Chan.newChan -- will be our new channel to the nodes
    newChan <- Chan.newChan -- the new channel for the WebSockets to send data to

    {- kill the stream from the webSockets to retire chan to the out chan. (messages will now queue in inChannel) -}
    killThread oldChannelToOutTid

    let bufferToOUt = filteredChan bufferChan outChannel (TransformerData (DeduplicationTState Nothing Map.empty))

    {- sync buffer to out (deduplication)-}
    bTid <- forkIO $ runFilteredChan bufferToOUt
    putStrLn "STARTED DEDUPLICATION !!!!!!!!! \n\n\n\n\n"

    {- redirect data from webSockets to retire to deduplication queue (buffer to out) -}
    oldChannelToBufferTid <- forkIO $ runPipedChan $ pipeChan inChannel bufferChan

    {- redirect data from webSockets to create to deduplication queue (buffer to out) -}
    newChannelToBufferTid <- forkIO $ runPipedChan $ pipeChan newChan bufferChan

    {- create the new WebSockets and their respective worker nodes -}
    newNodes <- foldM (\mp node@(SyncedWSNode _ name _ _ _ _) -> do
                                                               nn <- (mapToNewNodes node newChan)
                                                               return (Map.insert name nn mp)  ) Map.empty nodeMap

    {- Disconnect the old WebSockets and close kill threads  -}
    _ <- mapM (\(SyncedWSNode pid _ _ _ _ cmdIn) -> do (MVar.putMVar cmdIn Exit)) nodeMap

    {- todo NOW WE HAVE TO WAIT UNTIL THE NEW SOCKETS ARE CONNECTED... THEN KILL THE OLD SYNC-}
    threadDelay (4 * 1000 * 1000)

    {- kill the old pipe from the now retired/killed WebSockets to the buffer/deduplicator-}
    killThread oldChannelToBufferTid

    {- kill the deduplicator pipe since there is no need anymore (old WebSockets are retired/killed). messages are now queue in new chan-}
    killThread newChannelToBufferTid

    {- kill deduplicator to out-}
    killThread bTid

    putStrLn "FINISHED DEDUPLICATION !!!!!!!\n\n\n\n"

    {- now pipe newChan directly to the out chan -}
    newChannelToOutTid <- forkIO $ runPipedChan $ pipeChan newChan outChannel

    return (SyncedWSCluster outChannel newChan newNodes newChannelToOutTid)
--    let msgBuffer = Map.empty -- will be our buffer while we reconnect to websockets
--
mapToNewNodes :: SyncedWSNode -> Chan.Chan BL.ByteString -> IO (SyncedWSNode)
mapToNewNodes (SyncedWSNode _ _ _ _ (WSMinimalDefinition host path port onOpen) _) chan = runSocket (WSDefinition host path port chan onOpen)
mapToNewNodes (SyncedWSNode _ _ _ _ (WSDefinition host path port _ onOpen) _) chan = runSocket (WSDefinition host path port chan onOpen)


mkEmptyCluster :: IO (SyncedWSCluster)
mkEmptyCluster = do
                 inc <- Chan.newChan
                 outc <- Chan.newChan
                 spid <- forkIO $ runPipedChan $ pipeChan inc outc
                 return $ SyncedWSCluster outc inc (Map.empty) spid
--
addSockets :: WSDefinition -> SyncedWSCluster -> IO (SyncedWSCluster)
addSockets socketDef (SyncedWSCluster out inc cmap spid) = do
                               node <- runSocket (WSDefinition host path port out onOpen)
                               return (SyncedWSCluster out inc (Map.insert (syncedWSName node) node cmap) spid)
                               where host = wsHost socketDef
                                     path = wsPath socketDef
                                     port = wsPort socketDef
                                     onOpen = wsOnOpen socketDef


test2 :: IO ()
test2 = do
       cluster <- mkEmptyCluster
       cluster <- addSockets (WSMinimalDefinition "ws.kraken.com" "/" 443 K.subscribe)  cluster
       cluster <- addSockets (WSMinimalDefinition "ws.bitstamp.net" "/" 443 BIT.subscribe)  cluster
--       cluster2 <- addSockets (WSMinimalDefinition "ws.kraken.com" "/" 443) K.subscribe cluster
--       node <- runSocket (WSMinimalDefinition "ws.kraken.com" "/" 443) K.subscribe
       forkIO $ (\ cluster -> do
                    threadDelay 3000000
                    putStrLn "live reload"
                    cluster <- liveReconnect cluster
                    return ()
                    ) cluster
       putStrLn $ show $ fmap syncedWSPID (Map.lookup "ws.kraken.com" (clusterWSMap cluster))
       putStrLn $ show $ fmap syncedWSPID (Map.lookup "ws.bitstamp.net" (clusterWSMap cluster))
       let loop queue = do
                        msg <- Chan.readChan queue
                        putStrLn $ show msg
                        loop queue
       loop (clusterOutChannel cluster)
