module Exchange.Handler
  ( JsonDecoder
  , decodeAndEnQueueHandler
  , enqueueHandler
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Data.ByteString.Lazy    as BL

import           Exchange.Types
import           Exchange.Utils
import           Utils.WebSocket

type JsonDecoder a = (BL.ByteString -> Either String a) -- ^ will decode a bytestring into a datatype.

decodeAndEnQueueHandler :: JsonDecoder a -> C.Chan a -> WebsocketHandler
decodeAndEnQueueHandler decoder queue = (\_ msg -> decodeAndQueue decoder queue msg)

decodeAndQueue :: JsonDecoder a -> C.Chan a -> BL.ByteString -> IO ()
decodeAndQueue decoder queue msg =
  case decoder msg of
    Right orderMsg -> C.writeChan queue orderMsg
    Left err       -> printParseError err msg

enqueueHandler :: C.Chan BL.ByteString -> WebsocketHandler
enqueueHandler queue = (\_ msg -> C.writeChan queue msg)
