module Utils.WebSocket
  ( new
  , Host
  , WebsocketHandler
  , OnConnect
  , Path
  ) where

import qualified Control.Exception             as E
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as B
import qualified Network.Connection            as C
import qualified Network.WebSockets            as W

import           Control.Concurrent
import           Control.Monad                 (void)
import           Network.Socket
import           System.IO
import           Wuss

type Host = String

type Path = String

type WebsocketHandler = W.Connection -> BL.ByteString -> IO ()

type OnConnect = (W.Connection -> IO ())

data WebsocketHandle =
  WebsocketHandle
    { wsHost       :: Host
    , wsPath       :: Path
    , wsPort       :: PortNumber
    , wsHandler    :: WebsocketHandler
    , wsOnConnect  :: OnConnect
    , wsSubscribed :: Bool
    }

new :: Host -> Path -> PortNumber -> WebsocketHandler -> OnConnect -> IO ()
new host path port onMessage onOpen = void $ forkIO (runSecureClient host port path wsApp)
  where
    handle = WebsocketHandle host path port onMessage onOpen False
    wsApp = worker handle

worker :: WebsocketHandle -> W.Connection -> IO ()
worker handle connection
  | isSubscribed = do
    result <- E.try (W.receiveDataMessage connection) :: IO (Either W.ConnectionException W.DataMessage)
    case result of
      Left ex -> print ("Exception in Websocket connection " ++ show ex)
      Right (W.Binary val) -> onMessage connection val >> worker handle connection
      Right (W.Text val1 val2) -> onMessage connection val1 >> worker handle connection
  | otherwise = onOpen connection >> worker (handle {wsSubscribed = True}) connection
  where
    onMessage = wsHandler handle
    onOpen = wsOnConnect handle
    isSubscribed = wsSubscribed handle
