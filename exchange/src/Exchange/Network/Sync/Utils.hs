{-# LANGUAGE OverloadedStrings #-}
module Exchange.Network.Sync.Utils (
       runSocket
) where

import qualified Control.Concurrent.STM.TChan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.QSem as Sem
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLU
import qualified Network.WebSockets as WS
import Control.Concurrent
import Exchange.Network.Sync.Types
import Exchange.Network.Socket
import System.IO
import System.Exit
import Control.Monad.STM

runSocket :: WSDefinition -> IO (SyncedWSNode)
runSocket (WSMinimalDefinition host path port onOpen)  = do
    chan <- Chan.newTChanIO
    runSocket (WSDefinition host path port chan onOpen)
runSocket def@(WSDefinition host path port chan onOpen) = do
    con <- runSecureClient2 host path port onOpen
    commandChan <- MVar.newEmptyMVar
    commandIn <- MVar.newEmptyMVar
    sem <- Sem.newQSem 0
    pid <- forkFinally (worker con chan commandChan commandIn) (\ _ -> Sem.signalQSem sem)
    return $ SyncedWSNode pid host chan commandChan def commandIn sem

worker :: WS.Connection -> Chan.TChan BL.ByteString -> MVar.MVar SyncedWSNodeCommands -> MVar.MVar ClusterTONodeCommands -> IO ()
worker connection msgChan cmdChan cmdChanIn = MVar.putMVar cmdChan WorkerStarted >> workerLoop connection msgChan cmdChan cmdChanIn
           where workerLoop connection msgChan cmdChan cmdChanIn = do
                                                                   cmd <- MVar.tryTakeMVar cmdChanIn
                                                                   case cmd of
                                                                     (Just Exit) -> putStrLn "sending close" >> (WS.sendCloseCode connection 1000 Exit) >> exitSuccess
                                                                     Nothing     -> readFromSocket connection msgChan
                                                                   workerLoop connection msgChan cmdChan cmdChanIn


readFromSocket :: WS.Connection -> Chan.TChan BL.ByteString -> IO ()
readFromSocket connection msgChan = do
                            res <- E.try ( WS.receiveDataMessage connection) :: IO (Either WS.ConnectionException WS.DataMessage)
                            case res of
                                 Left ex                   -> (putStrLn $ "Exception in Websocket connection " ++ (show ex)) >> (hFlush stdout)
                                 Right (WS.Binary val)     -> atomically (Chan.writeTChan msgChan val) >> (return ())
                                 Right (WS.Text val1 val2) -> atomically (Chan.writeTChan msgChan val1) >> (return ())


instance WS.WebSocketsData ClusterTONodeCommands where
    toLazyByteString d =  BLU.pack $ show d
