{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

type ChainedChannel = IO ()

{- | Maybe ByteString determines whether a message needs to be sent or not. The map serves as a lookup table -}
data UniqueTransformer = UniqueTransformer (Maybe BL.ByteString) (Map.Map BL.ByteString BL.ByteString) deriving (Show)

{- | Makes use of the Unique transformer. If the message is set we will write to the sink and add the message to the lookup table -}
writeUnique :: Chan.Chan BL.ByteString -> UniqueTransformer -> IO (UniqueTransformer)
writeUnique chan (UniqueTransformer (Just msg) lookupMap) = Chan.writeChan chan msg >> (return $ UniqueTransformer Nothing (Map.insert msg msg lookupMap))
writeUnique chan (UniqueTransformer _ lookupMap)          = return (UniqueTransformer Nothing lookupMap)

{- | Will tell whether this message is unique or not. lookupFn should be a partially applied function which solely depends on the msg -}
applyUniqueFilterSink :: Chan.Chan BL.ByteString -> (BL.ByteString -> UniqueTransformer) -> IO (UniqueTransformer)
applyUniqueFilterSink source lookupFn = (liftM lookupFn (Chan.readChan source))

{- | Pipes messages from a source to a sink and makes sure that messages are not redundant -}
uniqueCChan :: Chan.Chan BL.ByteString -> Chan.Chan BL.ByteString -> UniqueTransformer -> IO (UniqueTransformer)
uniqueCChan source sink transformer = (applyUniqueFilterSink source (filterPresent transformer)) >>= (writeUnique sink)

runUniqueCChan :: IO (UniqueTransformer) -> IO ()
runUniqueCChan uniquePipe = uniquePipe >> runUniqueCChan uniquePipe

{- | is responsible for determining whether the message has already be sent or not. -}
filterPresent :: UniqueTransformer -> BL.ByteString -> UniqueTransformer
filterPresent (UniqueTransformer _ lookupMap) bs = case Map.lookup bs lookupMap of
                                                   (Just _) -> (UniqueTransformer Nothing lookupMap)
                                                   Nothing  -> (UniqueTransformer (Just bs) lookupMap)

{- | Creates a simple pipeline forwarding messages from a source to a sink -}
chainChannel :: Chan.Chan BL.ByteString -> Chan.Chan BL.ByteString -> ChainedChannel
chainChannel source sink = (Chan.readChan source) >>= (Chan.writeChan sink)

{- | simple wrapper which can be run asynchronously to always pipe forward -}
runCChan :: ChainedChannel -> IO ()
runCChan cChan = cChan >> runCChan cChan


{-
for ghci
source <- newChan :: IO (Chan ByteString)
sink <- newChan :: IO (Chan ByteString)
let mp = M.empty :: M.Map ByteString ByteString
writeChan source "test4"
let uniquePipe = uniqueCChan source sink (UniqueTransformer Nothing M.empty)
-}

--dummyLiveReload :: IO
--dummyLiveReload = do
--        {- this is our initial setting once we live reload-}
--
--        currentMsgInChan <- Chan.newChan :: IO (Chan BL.ByteString) -- current messages from websockets
--        bufferChan <- Chan.newChan :: IO (Chan BL.ByteString) -- man in the middle
--        outChan <- Chan.newChan :: IO (Chan BL.ByteString) -- propagates to cluster subscribers
--
--        let fromInToBuffer = chainChannel currentMsgInChan bufferChan
--        let fromBufferToOut = chainChannel bufferChan outChan
--
--        ibTid <- forkIO runCChan fromInToBuffer -- forward everything to buffer
--        boTid <- forkIO runCChan fromBufferToOut -- forward everything to out
--        {- start live reload -}
--
--        newMsgInChan <- Chan.newChan :: IO (Chan BL.ByteString) -- this is the new channel from the new sockets
--
--        let fromNewInToBuffer = chainChannel newMsgInChan bufferChan
--
--        nibTid <- forkIO runCChan fromNewInToBuffer -- connect to the buffer with the new messages as well
--
--        killThread boTid -- kill output stream to subscribers of cluster
--        {- now the buffer is filling with values from the old websockets and the new ones -}
--
--        {- now we need to use the unique transformer. I.e make sure we don't output the same values twice -}
--









syncMessages :: Chan.Chan BL.ByteString -> Chan.Chan BL.ByteString -> Chan.Chan BL.ByteString -> Map.Map BL.ByteString BL.ByteString -> IO ()
syncMessages oldChan newChan bufferChan msgMap = do
    putStrLn "syncing..." >> (hFlush stdout)
    msgFromOld <- Chan.readChan oldChan
    msgMap <- handleMessage bufferChan msgFromOld msgMap
    msgFromNew <- Chan.readChan newChan
    msgMap <- handleMessage bufferChan msgFromNew msgMap
    syncMessages oldChan newChan bufferChan msgMap

handleMessage :: Chan.Chan BL.ByteString -> BL.ByteString -> Map.Map BL.ByteString BL.ByteString -> IO (Map.Map BL.ByteString BL.ByteString)
handleMessage bufferChan msg msgMap = case Map.lookup msg msgMap of
                                      Nothing  -> do
                                                  Chan.writeChan bufferChan msg
                                                  return $ Map.insert msg msg msgMap
                                      (Just _) -> return msgMap


liveReconnect :: SyncedWSCluster -> IO (SyncedWSCluster)
liveReconnect cluster@(SyncedWSCluster outChannel inChannel nodeMap spid) = do
    bufferChan <- Chan.newChan -- will be our new channel to the nodes
    tempChannel <- Chan.newChan

    smPid <- forkIO $ syncMessages inChannel tempChannel bufferChan $ Map.empty

    newNodes <- foldM (\mp node@(SyncedWSNode _ name _ _ _ _) -> do
                                                               nn <- (mapToNewNodes node bufferChan)
                                                               return (Map.insert name nn mp)  ) Map.empty nodeMap

--    newNodes <- foldM (\mp node@(SyncedWSNode _ name _ _ _) -> liftM ((mapToNewNodes node bufferChan) >>= (Map.insert name)) mp ) Map.empty nodeMap
    {- we synchronize the old and new values and keep them unique -}
    _ <- mapM (\(SyncedWSNode pid _ _ _ _ cmdIn) -> do (MVar.putMVar cmdIn Exit)) nodeMap
    {- todo NOW WE HAVE TO WAIT UNTIL THE NEW SOCKETS ARE CONNECTED... THEN KILL THE OLD SYNC-}
    putStrLn "before killing"
    smth <- killThread spid -- kill the synchronizer
    putStrLn $ show smth
    putStrLn "after kiling sync spid"
    threadDelay 5000000
    putStrLn "afterDelay"


    newSpid <-forkIO $ syncQueues bufferChan outChannel
    {-todo now we need to clean up the retired websocket threads -}
    return (SyncedWSCluster outChannel bufferChan newNodes newSpid)
--    let msgBuffer = Map.empty -- will be our buffer while we reconnect to websockets
--
mapToNewNodes :: SyncedWSNode -> Chan.Chan BL.ByteString -> IO (SyncedWSNode)
mapToNewNodes (SyncedWSNode _ _ _ _ (WSMinimalDefinition host path port onOpen) _) chan = runSocket (WSDefinition host path port chan onOpen)
mapToNewNodes (SyncedWSNode _ _ _ _ (WSDefinition host path port _ onOpen) _) chan = runSocket (WSDefinition host path port chan onOpen)

syncQueues :: Chan.Chan BL.ByteString -> Chan.Chan BL.ByteString -> IO ()
syncQueues inc out = (Chan.readChan inc) >>= (Chan.writeChan out) >> syncQueues inc out

mkEmptyCluster :: IO (SyncedWSCluster)
mkEmptyCluster = do
                 inc <- Chan.newChan
                 outc <- Chan.newChan
                 spid <- forkIO $ syncQueues inc outc
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
                        putStrLn $ show $ length $ show msg
                        loop queue
       loop (clusterOutChannel cluster)
