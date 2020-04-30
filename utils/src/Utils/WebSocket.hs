module Utils.WebSocket
  ( runSecureClient
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
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets            as W
import qualified Network.WebSockets.Stream     as Stream

import           Control.Concurrent
import           Network.Socket
import           System.IO

type Host = String

type Path = String

type WebsocketHandler = W.Connection -> BL.ByteString -> IO ()

type OnConnect = (W.Connection -> IO ())

{- endpoint, port, queue, function applied at con open -}
runSecureClient :: Host -> Path -> PortNumber -> WebsocketHandler -> OnConnect -> IO ()
runSecureClient host path port onMessage onOpen = do
  context <- C.initConnectionContext
  connection <- C.connectTo context (connectionParams host port)
  stream <- Stream.makeStream (reader connection) (writer connection)
  newCon <- WS.newClientConnection stream host path connectionOptions []
  onOpen newCon
  forkIO $ worker newCon onMessage
  return ()

worker :: W.Connection -> WebsocketHandler -> IO ()
worker connection onMessage = do
  result <- E.try (WS.receiveDataMessage connection) :: IO (Either W.ConnectionException W.DataMessage)
  case result of
    Left ex -> print ("Exception in Websocket connection " ++ show ex) >> hFlush stdout >> worker connection onMessage
    Right (W.Binary val) -> onMessage connection val >> worker connection onMessage
    Right (W.Text val1 val2) -> onMessage connection val1 >> worker connection onMessage

{- connection config -}
connectionParams :: String -> PortNumber -> C.ConnectionParams
connectionParams host port =
  C.ConnectionParams
    { C.connectionHostname = host
    , C.connectionPort = port
    , C.connectionUseSecure = Just tlsSettings
    , C.connectionUseSocks = Nothing
    }

tlsSettings :: C.TLSSettings
tlsSettings =
  C.TLSSettingsSimple
    {C.settingDisableCertificateValidation = False, C.settingDisableSession = False, C.settingUseServerName = False}

reader :: C.Connection -> IO (Maybe BS.ByteString)
reader connection = fmap Just (C.connectionGetChunk connection)

writer :: C.Connection -> Maybe BL.ByteString -> IO ()
writer connection = maybe (return ()) (C.connectionPut connection . BL.toStrict)

connectionOptions :: WS.ConnectionOptions
connectionOptions = WS.defaultConnectionOptions

unpackMsg :: Maybe BS.ByteString -> BS.ByteString
unpackMsg Nothing  = error "couldn't parse msg"
unpackMsg (Just x) = x
