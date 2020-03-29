{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Exchange.Bitstamp.Contract.Websocket (
       Message
) where

import Data.ByteString
import GHC.Generics
import Data.Aeson
import Data.ByteString
import Exchange.Types
import Finance.Types
import qualified Data.Aeson.Types as AI
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

textToStrict :: (TL.Text -> ByteString)
textToStrict = (BL.toStrict . TLE.encodeUtf8)

textArrayToStrict :: ([[TL.Text]] -> [[ByteString]])
textArrayToStrict = Prelude.map (Prelude.map (BL.toStrict . TLE.encodeUtf8))

byteStringToDouble :: ByteString -> Double
byteStringToDouble bs = read (BC.unpack bs) :: Double

byteStringToInteger :: ByteString -> Int
byteStringToInteger bs = read (BC.unpack bs) :: Int

data Message  =  SubscribeMessage {
     subMsgEvent          :: !ByteString
    ,subMsgChannel        :: !ByteString
}  | UnsubscribeMessage {
     unsubMsgEvent        :: !ByteString
    ,unsubMsgChannel      :: !ByteString
}  | FullOrderBookMessage {
     fullOrderBookEvent   :: !ByteString
    ,fullOrderBookChannel :: !ByteString
    ,fullOrderMsgData     :: !FullOrderBookData
}  | ForcedReconnection {
     forcedReconnectEvent :: !ByteString
} deriving (Show)

data FullOrderBookData = FullOrderBookData {
     fOrderBookBids          :: ![[ByteString]]
    ,fOrderBookAsks          :: ![[ByteString]]
    ,fOrderMsgTimestamp      :: !ByteString
    ,fOrderMsgMicroTimestamp :: !ByteString
} deriving (Show)

parseSubscribeMessage :: Value -> AI.Parser Message
parseSubscribeMessage (Object object) = SubscribeMessage
                                        <$> fmap textToStrict (object .: "event")
                                        <*> fmap textToStrict (object .: "channel")

parseUnsubscribeMessage :: Value -> AI.Parser Message
parseUnsubscribeMessage (Object object)  = UnsubscribeMessage
                                           <$> fmap textToStrict (object .: "event")
                                           <*> fmap textToStrict (object .: "channel")

parseFullOrderBookMessage :: Value -> AI.Parser Message
parseFullOrderBookMessage (Object object) = FullOrderBookMessage
                                            <$> fmap textToStrict (object .: "event")
                                            <*> fmap textToStrict (object .: "channel")
                                            <*> object .: "data"

parseForcedReconnection :: Value -> AI.Parser Message
parseForcedReconnection (Object object) = ForcedReconnection
                                          <$> fmap textToStrict (object .: "event")

instance FromJSON Message
      where parseJSON (Object object) = case (HML.lookup "event"  object) of
                                        Just (String "bts:subscription_succeeded") -> (parseSubscribeMessage (Object object))
                                        Just (String "bts:unsubscribe")            -> (parseUnsubscribeMessage (Object object))
                                        Just (String "data")                       -> (parseFullOrderBookMessage (Object object))
                                        Just (String "bts:request_reconnect")      -> (parseForcedReconnection (Object object))
                                        _                                          -> fail $ "Unknown message format. Message sent from Bitstamp could not be parsed\n" ++ (show object)
            parseJSON smth@(_)        = fail $ "Unknown message format. Message sent from Bitstamp could not be parsed\n" ++ (show smth)

instance FromJSON FullOrderBookData where
      parseJSON (Object object) = FullOrderBookData
                                  <$> fmap (textArrayToStrict) (object .: "asks")
                                  <*> fmap (textArrayToStrict) (object .: "bids")
                                  <*> fmap textToStrict (object .: "timestamp")
                                  <*> fmap textToStrict (object .: "microtimestamp")

instance ExchangeOrder Message where
  toOrder message@(FullOrderBookMessage _ _ _) = (Prelude.map (\(price:qty:[]) -> AskOrder (mapToBaseOrder currencyPair price qty msgTimestamp)) asksArray) ++ (Prelude.map (\(price:qty:[]) -> BidOrder (mapToBaseOrder currencyPair price qty msgTimestamp)) bidsArray)
                where msgTimestamp = fOrderMsgMicroTimestamp (fullOrderMsgData message)
                      currencyPair = getCurrencyPairFromMessage message
                      asksArray    = fOrderBookBids (fullOrderMsgData message)
                      bidsArray    = fOrderBookAsks (fullOrderMsgData message)
  toOrder _ = []

instance (ExchangeOrder a) => ExchangeOrder (Maybe a) where
  toOrder (Just message) = toOrder message
  toOrder Nothing        = []

mapToBaseOrder :: CurrencyPair -> ByteString -> ByteString -> ByteString -> BaseOrder
mapToBaseOrder currency price qty timestamp = BaseOrder Bitstamp currency (byteStringToDouble price) (byteStringToDouble qty) (byteStringToInteger timestamp)


getCurrencyPairFromMessage :: Message -> CurrencyPair
getCurrencyPairFromMessage (FullOrderBookMessage _ channel _) = getCurrencyPairFromChannel $ BC.unpack $ channel

getCurrencyPairFromChannel :: String -> CurrencyPair
getCurrencyPairFromChannel "diff_order_book_ltcusd" = LTCUSD
getCurrencyPairFromChannel "diff_order_book_bchusd" = BCHUSD
getCurrencyPairFromChannel "diff_order_book_ethusd" = ETHUSD
getCurrencyPairFromChannel "diff_order_book_xrpusd" = XRPUSD
