module Exchange.Utils
  ( JsonDecoder
  , MessageHandler
  , defaultHandler
  , orderFeedHandler
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as BL

import           Exchange.Types
import           Finance.Types
import           System.IO

type JsonDecoder a = (BL.ByteString -> Either String a) -- ^ will decode a bytestring into a datatype.

type MessageHandler a = (C.Chan a -> JsonDecoder a -> BL.ByteString -> IO ()) -- ^ takes a queue, a jsondecoder (maps from bytestring to some datatype) a bytestring and will perform some IO action on the parsed message

{- | Does not care about messages which should not be converted into orders... (will evaluate to empty array)
 Can be partially applied (Chan and JsonDecoder and the be used for feed subscription -}
orderFeedHandler :: (ExchangeOrder a) => C.Chan [Order] -> JsonDecoder a -> BL.ByteString -> IO ()
orderFeedHandler queue jsonDecoder msg =
  case jsonDecoder msg of
    Right orderMsg -> C.writeChan queue $ toOrder orderMsg
    Left err       -> printParseError err msg

defaultHandler :: C.Chan a -> JsonDecoder a -> BL.ByteString -> IO ()
defaultHandler queue jsonDecoder msg =
  case jsonDecoder msg of
    Right parsedMsg -> C.writeChan queue parsedMsg
    Left err        -> printParseError err msg

printParseError :: String -> BL.ByteString -> IO ()
printParseError err msg =
  putStrLn ("Error parsing JSON message:\n\t" ++ err ++ "\n\ttried to parse: \n\t" ++ show msg) >> hFlush stdout
