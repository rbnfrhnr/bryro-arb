module Exchange.Utils
  ( JsonDecoder
  , MessageHandler
  , byteStringToDouble
  , byteStringToInteger
  , defaultHandler
  , orderFeedHandler
  , textArrayToStrict
  , textStrictToStrict
  , textToStrict
  ) where

import qualified Control.Concurrent.Chan as C
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

import           Data.ByteString
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
  Prelude.putStrLn ("Error parsing JSON message:\n\t" ++ err ++ "\n\ttried to parse: \n\t" ++ show msg) >> hFlush stdout

textStrictToStrict :: (T.Text -> ByteString)
textStrictToStrict = BL.toStrict . TLE.encodeUtf8 . TL.fromStrict

textToStrict :: (TL.Text -> ByteString)
textToStrict = BL.toStrict . TLE.encodeUtf8

textArrayToStrict :: ([[TL.Text]] -> [[ByteString]])
textArrayToStrict = Prelude.map (Prelude.map (BL.toStrict . TLE.encodeUtf8))

byteStringToDouble :: ByteString -> Double
byteStringToDouble bs = read (BC.unpack bs) :: Double

byteStringToInteger :: ByteString -> Int
byteStringToInteger bs = read (BC.unpack bs) :: Int
