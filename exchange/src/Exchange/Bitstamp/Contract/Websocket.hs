{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Exchange.Bitstamp.Contract.Websocket (
       Message
      ,SubscribeMessage
      ,FullOrderBookMessage
) where

import Data.ByteString
import GHC.Generics
import Data.Aeson
import Data.Text.Lazy.Encoding as TLE
import Data.ByteString
import Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

textToStrict :: (TL.Text -> ByteString)
textToStrict = (BL.toStrict . TLE.encodeUtf8)

textArrayToStrict :: ([[TL.Text]] -> [[ByteString]])
textArrayToStrict = Prelude.map (Prelude.map (BL.toStrict . TLE.encodeUtf8))

data SubscribeMessage = SubMessage {
     subMsgEvent   :: !ByteString
    ,subMsgChannel :: !ByteString
} deriving (Show)

data UnsubscribeMessage = UnsubMessage {
     unsubMsgEvent   :: !ByteString
    ,unsubMsgChannel :: !ByteString
} deriving (Show)

data ForcedReconnection = Reconnection {
     forcedReconnectEvent   :: !ByteString
    ,forcedReconnectChannel :: !ByteString
    ,forcedMsgData          :: !ByteString
} deriving (Show)

data FullOrderBookMessage = FOrderBookMessage {
     fOrderBookEvent         :: !ByteString
    ,fOrderBookChannel       :: !ByteString
    ,fOrderMsgData           :: !FullOrderBookData
} deriving (Show)

data FullOrderBookData = FullOrderBookData {
     fOrderBookBids          :: ![[ByteString]]
    ,fOrderBookAsks          :: ![[ByteString]]
    ,fOrderMsgTimestamp      :: !ByteString -- | yes.. Bytestring
    ,fOrderMsgMicroTimestamp :: !ByteString
} deriving (Show)

--data SubscriptionData = SubscriptionData {
--     channel :: !MayByteString
--} deriving (Show)

data Message =  SubscribeMessage | UnsubscribeMessage | FullOrderBookMessage deriving (Show, Generic)


instance FromJSON Message
      where parseJSON (Object object) = do
                                        event <- "data"
                                        case event of
                                            "diff_order_book_ltcusd" -> return (parseJSON object :: FullOrderBookMessage)
--instance FromJSON SubscriptionData
--      where parseJSON (Object object) = do
--                                        sChannel <- fmap textToStrict (object .: "channel")
--                                        return (SubscriptionData sChannel)

instance FromJSON FullOrderBookData
      where parseJSON (Object object) = do
                                        asks <- fmap (textArrayToStrict) (object .: "asks")
                                        bids <- fmap (textArrayToStrict) (object .: "bids")
                                        timestamp      <- fmap textToStrict $ object .: "timestamp"
                                        microtimestamp <- fmap textToStrict $ object .: "microtimestamp"
                                        return (FullOrderBookData asks bids timestamp microtimestamp)

instance FromJSON FullOrderBookMessage
      where parseJSON (Object object) = do
                                        event          <- fmap textToStrict $ object .: "event"
                                        channel        <- fmap textToStrict $ object .: "channel"
                                        msgData        <- object .: "data"
                                        return (FOrderBookMessage event channel msgData)

instance FromJSON ForcedReconnection
      where parseJSON (Object object) = do
                                        event   <- fmap textToStrict $ object .: "event"
                                        channel <- fmap textToStrict $ object .: "channel"
                                        msgData <- fmap textToStrict $ object .: "data"
                                        return (Reconnection event channel msgData)

instance FromJSON UnsubscribeMessage
      where parseJSON (Object object) = do
                                        event   <- fmap textToStrict $ object .: "event"
                                        channel <- fmap textToStrict $ object .: "channel"
                                        return (UnsubMessage event channel)

instance FromJSON SubscribeMessage
      where parseJSON (Object object) = do
                                        event   <- fmap textToStrict $ object .: "event"
                                        channel <- fmap textToStrict $ object .: "channel"
                                        return (SubMessage event channel)
--                                        return (SubMessage event (SubscriptionData ""))

--instance FromJSON Message