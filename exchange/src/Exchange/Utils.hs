module Exchange.Utils
  ( byteStringToDouble
  , byteStringToInteger
  , printParseError
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
