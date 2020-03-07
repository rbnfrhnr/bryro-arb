{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Exchange.Bitstamp.Contract.Websocket (
       Message
) where

import Data.ByteString
import GHC.Generics
import Data.Aeson
import Data.ByteString
import qualified Data.Aeson.Types as AI
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

textToStrict :: (TL.Text -> ByteString)
textToStrict = (BL.toStrict . TLE.encodeUtf8)

textArrayToStrict :: ([[TL.Text]] -> [[ByteString]])
textArrayToStrict = Prelude.map (Prelude.map (BL.toStrict . TLE.encodeUtf8))

data Message = SubscribeMessage {
     subMsgEvent   :: !ByteString
    ,subMsgChannel :: !ByteString
} | UnsubscribeMessage {
     unsubMsgEvent   :: !ByteString
    ,unsubMsgChannel :: !ByteString
} | FullOrderBookMessage {
     fullOrderBookEvent         :: !ByteString
    ,fullOrderBookChannel       :: !ByteString
    ,fullOrderMsgData           :: !FullOrderBookData
} | ForcedReconnection {
     forcedReconnectEvent :: !ByteString
} | SomeMessage ByteString deriving (Show)

data FullOrderBookData = FullOrderBookData {
     fOrderBookBids          :: ![[ByteString]]
    ,fOrderBookAsks          :: ![[ByteString]]
    ,fOrderMsgTimestamp      :: !ByteString -- | yes.. Bytestring
    ,fOrderMsgMicroTimestamp :: !ByteString
} deriving (Show)

instance FromJSON Message
      where parseJSON (Object object) = case (HML.lookup "event"  object) of
                                        Just (String "bts:subscription_succeeded") -> SubscribeMessage <$> fmap textToStrict (object .: "event") <*> fmap textToStrict (object .: "channel")
                                        Just (String "bts:unsubscribe")            -> UnsubscribeMessage <$> fmap textToStrict (object .: "event") <*> fmap textToStrict (object .: "channel")
                                        Just (String "data")                       -> orderMessage (Object object)
                                        Just (String "bts:request_reconnect")      -> ForcedReconnection <$> fmap textToStrict (object .: "event")
                                        _                                          -> fail "Unknown message format. Message sent from Bitstamp could not be parsed"

orderMessage :: Value -> AI.Parser Message
orderMessage (Object object) = do
                               event   <- fmap textToStrict $ object .: "event"
                               channel <- fmap textToStrict $ object .: "channel"
                               msgData <- messageData (HML.lookup "data" object)
                               return (FullOrderBookMessage event channel msgData)

messageData :: Maybe Value -> AI.Parser FullOrderBookData
messageData (Just (Object object))  = do
                                      asks           <- fmap (textArrayToStrict) (object .: "asks")
                                      bids           <- fmap (textArrayToStrict) (object .: "bids")
                                      timestamp      <- fmap textToStrict $ object .: "timestamp"
                                      microtimestamp <- fmap textToStrict $ object .: "microtimestamp"
                                      return (FullOrderBookData asks bids timestamp microtimestamp)
