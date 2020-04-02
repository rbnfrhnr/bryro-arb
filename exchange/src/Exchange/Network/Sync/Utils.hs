{-# LANGUAGE OverloadedStrings #-}
module Exchange.Network.Sync.Utils (
       runSocket
) where

import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLU
--import qualified Data.ByteString.UTF8 as BSU
import qualified Network.WebSockets as WS
import Control.Concurrent
import Exchange.Network.Sync.Types
import Exchange.Network.Socket
import System.IO
import System.Exit

import Exchange.Kraken.Utils as K


runSocket :: WSDefinition -> IO (SyncedWSNode)
runSocket (WSMinimalDefinition host path port onOpen)  = do
    chan <- Chan.newChan
    runSocket (WSDefinition host path port chan onOpen)
runSocket def@(WSDefinition host path port chan onOpen) = do
    con <- runSecureClient2 host path port onOpen
    commandChan <- Chan.newChan
    commandIn <- MVar.newEmptyMVar
    pid <- forkIO $ worker con chan commandChan commandIn
    return $ SyncedWSNode pid host chan commandChan def commandIn

worker :: WS.Connection -> Chan.Chan BL.ByteString -> Chan.Chan SyncedWSNodeCommands -> MVar.MVar ClusterTONodeCommands -> IO ()
worker connection msgChan cmdChan cmdChanIn = do
                                              cmd <- MVar.tryTakeMVar cmdChanIn
                                              case cmd of
                                                  (Just Exit) -> putStrLn "sending close" >> (WS.sendCloseCode connection 1000 Exit) >> exitSuccess
                                                  Nothing     -> readFromSocket connection msgChan
                                              worker connection msgChan cmdChan cmdChanIn

readFromSocket :: WS.Connection -> Chan.Chan BL.ByteString -> IO ()
readFromSocket connection msgChan = do
                            res <- E.try ( WS.receiveDataMessage connection) :: IO (Either WS.ConnectionException WS.DataMessage)
                            case res of
                                 Left ex                   -> (putStrLn $ "Exception in Websocket connection " ++ (show ex)) >> (hFlush stdout)
                                 Right (WS.Binary val)     -> Chan.writeChan msgChan val
                                 Right (WS.Text val1 val2) -> Chan.writeChan msgChan val1


instance WS.WebSocketsData ClusterTONodeCommands where
    toLazyByteString d =  BLU.pack $ show d


test :: IO ()
test = do
       node <- runSocket (WSMinimalDefinition "ws.kraken.com" "/" 443 K.subscribe)

       putStrLn $ show (syncedWSPID node)
       let loop queue = do
                        msg <- Chan.readChan queue
                        putStrLn $ show msg
                        loop queue
       loop (syncedCommunicationOut node)