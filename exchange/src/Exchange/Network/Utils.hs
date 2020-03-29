module Exchange.Network.Utils (
       orderFeedHandler
) where

import qualified Control.Concurrent.Chan as C
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Finance.Types
import Exchange.Types

orderFeedHandler :: (ExchangeOrder a) => C.Chan [Order] -> (BL.ByteString ->  Either String a) -> BL.ByteString -> IO ()
orderFeedHandler queue jsonDecoder msg = case jsonDecoder msg of
                                         Right orderMsg -> C.writeChan queue $ toOrder orderMsg
                                         Left err       -> putStrLn $ "Error parsing JSON message:\n\t" ++ (show err)
