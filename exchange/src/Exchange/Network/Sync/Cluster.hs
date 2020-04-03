{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Exchange.Network.Sync.Cluster (
--       test2
) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Control.Concurrent.STM.TChan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.QSem as Sem
import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Control.Exception.Base
import Exchange.Network.Sync.Types
import Exchange.Network.Sync.Utils
import Exchange.Network.Sync.ChanPipe
import System.IO
import Finance.Types

import Exchange.Kraken.Utils as K
import Exchange.Bitstamp.Utils as BIT

fetchAll :: Chan.TChan a -> IO [a]
fetchAll chan = do
                maybeBs <- (atomically (Chan.tryReadTChan chan))
                case maybeBs of
                    (Just bs) -> liftM (bs :) (fetchAll chan)
                    Nothing   -> return ([])


liveReconnect :: (WriteOut SimpleTState o, WriteOut DeduplicationTState o) => SyncedWSCluster o -> IO (SyncedWSCluster o )
liveReconnect cluster@(SyncedWSCluster outChannel inChannel nodeMap oldChannelToOutTid sem) = do
    killThread oldChannelToOutTid -- nothing should write to outChannel now...
    Sem.waitQSem sem

    newChan <- Chan.newTChanIO
    {- starting the new webSockets -}
    newNodes <- foldM (\mp node@(SyncedWSNode _ name _ _ _ _ _) -> do
                                                               nn <- (mapToNewNodes node newChan)
                                                               MVar.takeMVar (syncedCommandOut nn)
                                                               return (Map.insert name nn mp)  ) Map.empty nodeMap
--    threadDelay (10000)

    {- Disconnect the old WebSockets and close kill threads  -}
    _ <- mapM (\(SyncedWSNode pid _ _ _ _ cmdIn nodeSem) -> (MVar.putMVar cmdIn Exit) >> (Sem.waitQSem nodeSem)) nodeMap

--    threadDelay (5000)

    {- all the old data -}
    oldData <- fetchAll inChannel


    {- write all the old data to the out line and update our Deduplicator -}
    dedupeState <- foldM (\dState msg -> (updateTState dState msg) >>= (writeOut outChannel)) (TransformerData (DeduplicationTState Nothing Map.empty)) oldData

--    {- Snapshot from all the new data since startup -}
    newDataSnapshot <-fetchAll newChan
    {- write all from the new websockets snapshot and make sure messages are sent only once with deduplicator -}
    _ <- foldM (\dState msg -> (updateTState dState msg) >>= (writeOut outChannel)) dedupeState newDataSnapshot

    (newChannelToOutTid, newSem) <- runFilteredChanAsync (filteredChan newChan outChannel (TransformerData (SimpleTState Nothing)))

    return (SyncedWSCluster outChannel newChan newNodes newChannelToOutTid newSem)


mapToNewNodes :: SyncedWSNode -> Chan.TChan BL.ByteString -> IO (SyncedWSNode)
mapToNewNodes (SyncedWSNode _ _ _ _ (WSMinimalDefinition host path port onOpen) _ _) chan = runSocket (WSDefinition host path port chan onOpen)
mapToNewNodes (SyncedWSNode _ _ _ _ (WSDefinition host path port _ onOpen) _ _) chan = runSocket (WSDefinition host path port chan onOpen)


mkEmptyCluster :: (WriteOut SimpleTState o ) => IO (SyncedWSCluster o)
mkEmptyCluster = do
                 inc <- Chan.newTChanIO
                 outc <- Chan.newTChanIO
                 (spid, sem) <- runFilteredChanAsync (filteredChan inc outc (TransformerData (SimpleTState Nothing)))
                 return $ SyncedWSCluster outc inc (Map.empty) spid sem


addSockets :: (WriteOut SimpleTState o) => WSDefinition -> SyncedWSCluster o -> IO (SyncedWSCluster o)
addSockets socketDef (SyncedWSCluster out inc cmap spid sem) = do
                               node <- runSocket (WSDefinition host path port inc onOpen)
                               return (SyncedWSCluster out inc (Map.insert (syncedWSName node) node cmap) spid sem)
                               where host = wsHost socketDef
                                     path = wsPath socketDef
                                     port = wsPort socketDef
                                     onOpen = wsOnOpen socketDef
