{-# LANGUAGE OverloadedStrings #-}

module Exchange.Bitstamp.Contract.Websocket
  ( BitstampMessage
  ) where

import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Lazy       as HML
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

import           Data.Aeson
import           Data.ByteString
import           Exchange.Types
import           Exchange.Utils
import           Finance.Types

data BitstampMessage
  = OrderBookUpdateMessage OrderBookUpdatePayload
  | UnsubscribeMessage UnsubscribePayload
  | SubscribeMessage SubscribePayload
  | ForcedReconnectionMessage
  deriving (Show)

data SubscribePayload =
  SubscribePayload
    { subMsgEvent   :: !ByteString
    , subMsgChannel :: !ByteString
    }
  deriving (Show)

data UnsubscribePayload =
  UnsubscribePayload
    { unsubMsgEvent   :: !ByteString
    , unsubMsgChannel :: !ByteString
    }
  deriving (Show)

data OrderBookUpdatePayload =
  OrderBookUpdatePayload
    { fullOrderBookEvent   :: !ByteString
    , fullOrderBookChannel :: !ByteString
    , fullOrderMsgData     :: !OrderBookUpdateData
    }
  deriving (Show)

data OrderBookUpdateData =
  OrderBookUpdateData
    { fOrderBookBids          :: ![[ByteString]]
    , fOrderBookAsks          :: ![[ByteString]]
    , fOrderMsgTimestamp      :: !ByteString
    , fOrderMsgMicroTimestamp :: !ByteString
    }
  deriving (Show)

instance FromJSON OrderBookUpdatePayload where
  parseJSON (Object object) =
    OrderBookUpdatePayload <$> fmap textToStrict (object .: "event") <*> fmap textToStrict (object .: "channel") <*>
    object .: "data"

instance FromJSON OrderBookUpdateData where
  parseJSON (Object object) =
    OrderBookUpdateData <$> fmap textArrayToStrict (object .: "asks") <*> fmap textArrayToStrict (object .: "bids") <*>
    fmap textToStrict (object .: "timestamp") <*>
    fmap textToStrict (object .: "microtimestamp")

instance FromJSON SubscribePayload where
  parseJSON (Object object) =
    SubscribePayload <$> fmap textToStrict (object .: "event") <*> fmap textToStrict (object .: "channel")

instance FromJSON UnsubscribePayload where
  parseJSON (Object object) =
    UnsubscribePayload <$> fmap textToStrict (object .: "event") <*> fmap textToStrict (object .: "channel")

instance FromJSON BitstampMessage where
  parseJSON (Object object) =
    case HML.lookup "event" object of
      Just (String "bts:subscription_succeeded") -> SubscribeMessage <$> parseJSON (Object object)
      Just (String "bts:unsubscribe") -> UnsubscribeMessage <$> parseJSON (Object object)
      Just (String "bts:request_reconnect") -> return ForcedReconnectionMessage
      Just (String "data") -> OrderBookUpdateMessage <$> parseJSON (Object object)
      _ -> fail $ "Unknown message format. Message sent from Bitstamp could not be parsed\n" ++ show object

instance ExchangeOrder BitstampMessage where
  toOrder (OrderBookUpdateMessage message) =
    Prelude.map (\[price, qty] -> AskOrder (mapToBaseOrder currencyPair price qty msgTimestamp)) asksArray ++
    Prelude.map (\[price, qty] -> BidOrder (mapToBaseOrder currencyPair price qty msgTimestamp)) bidsArray
    where
      msgTimestamp = fOrderMsgMicroTimestamp (fullOrderMsgData message)
      currencyPair = getCurrencyPairFromMessage message
      asksArray = fOrderBookBids (fullOrderMsgData message)
      bidsArray = fOrderBookAsks (fullOrderMsgData message)
  toOrder _ = []

instance (ExchangeOrder a) => ExchangeOrder (Maybe a) where
  toOrder (Just message) = toOrder message
  toOrder Nothing        = []

mapToBaseOrder :: CurrencyPair -> ByteString -> ByteString -> ByteString -> BaseOrder
mapToBaseOrder currency price qty timestamp =
  BaseOrder Bitstamp currency (byteStringToDouble price) (byteStringToDouble qty) (byteStringToInteger timestamp)

getCurrencyPairFromMessage :: OrderBookUpdatePayload -> CurrencyPair
getCurrencyPairFromMessage (OrderBookUpdatePayload _ channel _) = getCurrencyPairFromChannel $ BC.unpack channel

getCurrencyPairFromChannel :: String -> CurrencyPair
getCurrencyPairFromChannel "diff_order_book_ltcusd" = LTCUSD
getCurrencyPairFromChannel "diff_order_book_bchusd" = BCHUSD
getCurrencyPairFromChannel "diff_order_book_ethusd" = ETHUSD
getCurrencyPairFromChannel "diff_order_book_xrpusd" = XRPUSD
