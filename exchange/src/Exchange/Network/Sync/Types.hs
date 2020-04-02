{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Exchange.Network.Sync.Types (
       ClusterTONodeCommands(..)
      ,MessageHandler
      ,OnWSOpen
      ,SyncedWSCluster(..)
      ,SyncedWSClusterOut
      ,SyncedWSNode(..)
      ,SyncedWSNodeCommands(..)
      ,SyncedWSNodeName
      ,WSDefinition(..)

      ,toSyncedWSClusterOutData
) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import qualified Network.WebSockets as WS
import qualified Network.Socket as NS
import Data.ByteString
import Control.Concurrent

{- | SyncedWSCluster allows for differently instructed SyncedWSNodes. This means that it is allowed that each
     of the sockets produce a different type of data. However, they all need to be able to transform into the
     SyncedWSCluster output data type. This is enforced with ExistentialQuantification for all nodes
-}
data SyncedWSCluster  = SyncedWSCluster {
     clusterOutChannel :: !(Chan.Chan BL.ByteString)
    ,clusterInChannel  :: !(Chan.Chan BL.ByteString)
    ,clusterWSMap      :: !(Map.Map SyncedWSNodeName (SyncedWSNode)) -- ^ as
    ,clusterSyncPID    :: !ThreadId
}

{- | Type parameter i denotes the type which the nodes produce/emit and o describes the unified type which will be
     communicated to the cluster subscribes
 -}
class SyncedWSClusterOut i o where
      toSyncedWSClusterOutData :: i -> o

type SyncedWSNodeName = String

{- | SyncedWSNode is a wrapper for a Websocket connection which will later be run in a cluster of multiple websocket
     connections. Type parameter o allows to define the type which the websocket will emit.
-}
data SyncedWSNode =  SyncedWSNode {
     syncedWSPID            :: !ThreadId                         -- ^ process ID after fork
    ,syncedWSName           :: !SyncedWSNodeName                 -- ^ name of the websocket given by the user
    ,syncedCommunicationOut :: !(Chan.Chan BL.ByteString)                    -- ^ channel which the websocket uses to communicate incoming messages
    ,syncedCommandOut       :: !(Chan.Chan SyncedWSNodeCommands) -- ^ channel to allow to give commands to the cluster
    ,syncedWSDefinition     :: !WSDefinition
    ,syncedWSCommandIn      :: !(MVar.MVar ClusterTONodeCommands)
}

data SyncedWSNodeCommands = RestartNode ThreadId | ShutdownNode | RestartAll
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
    ,outChan :: !(Chan.Chan BL.ByteString)
    ,wsOnOpen :: !OnWSOpen
}

type MessageHandler a = (BL.ByteString -> a)
type OnWSOpen = (WS.Connection -> IO ())