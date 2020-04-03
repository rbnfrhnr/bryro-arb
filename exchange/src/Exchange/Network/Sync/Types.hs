{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Exchange.Network.Sync.Types (
       ClusterTONodeCommands(..)
      ,MessageHandler
      ,OnWSOpen
      ,SyncedWSCluster(..)
      ,SyncedWSNode(..)
      ,SyncedWSNodeCommands(..)
      ,SyncedWSNodeName
      ,WSDefinition(..)
) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as Chan
import qualified Control.Concurrent.QSem as Sem
import qualified Network.WebSockets as WS
import qualified Network.Socket as NS
import Data.ByteString
import Control.Concurrent

{- | SyncedWSCluster allows for differently instructed SyncedWSNodes. This means that it is allowed that each
     of the sockets produce a different type of data. However, they all need to be able to transform into the
     SyncedWSCluster output data type. This is enforced with ExistentialQuantification for all nodes
-}
data SyncedWSCluster o  = SyncedWSCluster {
     clusterOutChannel :: !(Chan.TChan o)
    ,clusterInChannel  :: !(Chan.TChan BL.ByteString)
    ,clusterWSMap      :: !(Map.Map SyncedWSNodeName (SyncedWSNode))
    ,clusterSyncPID    :: !ThreadId -- ^ ThreadId of the syncing thread between clusterInChannel and clusterOutChannel
    ,clusterSyncSem    :: !Sem.QSem -- ^ Semaphore for awaiting killing of syncing thread (Thread referenced by clusterSyncPID)
}

class SyncedWSClusterForwardParser i o where
    transformInputToOut :: i -> o

type SyncedWSNodeName = String

{- | SyncedWSNode is a wrapper for a Websocket connection which will later be run in a cluster of multiple websocket
     connections. Type parameter o allows to define the type which the websocket will emit.
-}
data SyncedWSNode =  SyncedWSNode {
     syncedWSPID            :: !ThreadId                          -- ^ process ID after fork
    ,syncedWSName           :: !SyncedWSNodeName                  -- ^ name of the websocket given by the user
    ,syncedCommunicationOut :: !(Chan.TChan BL.ByteString)        -- ^ channel which the websocket uses to communicate incoming messages
    ,syncedCommandOut       :: !(MVar.MVar SyncedWSNodeCommands) -- ^ channel to allow to give commands to the cluster
    ,syncedWSDefinition     :: !WSDefinition                      -- ^ the definition of the node. Used by the cluster to create the new instance
    ,syncedWSCommandIn      :: !(MVar.MVar ClusterTONodeCommands) -- ^ channel to talk to the node.
    ,syncedWSSem            :: !Sem.QSem                          -- ^ Sem for awaiting worker thread killing
}

data SyncedWSNodeCommands = RestartNode ThreadId | ShutdownNode | RestartAll | WorkerStarted
data ClusterTONodeCommands = Exit deriving (Show)

data WSDefinition = WSMinimalDefinition {
     wsHost :: !String
    ,wsPath :: !String
    ,wsPort :: !NS.PortNumber
    ,wsOnOpen :: !OnWSOpen
} | WSDefinition {
     wsHost  :: !String
    ,wsPath  :: !String
    ,wsPort  :: !NS.PortNumber
    ,outChan :: !(Chan.TChan BL.ByteString)
    ,wsOnOpen :: !OnWSOpen
}

type MessageHandler a = (BL.ByteString -> a)
type OnWSOpen = (WS.Connection -> IO ())