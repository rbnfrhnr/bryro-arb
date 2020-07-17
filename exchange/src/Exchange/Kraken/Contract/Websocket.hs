{-# LANGUAGE OverloadedStrings #-}

module Exchange.Kraken.Contract.Websocket
  ( KrakenMessage
  ) where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Lazy       as HML
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector             as V

import           Data.Aeson.Types
import           Exchange.Types
import           Exchange.Utils
import           Finance

data KrakenMessage
  = OrderBookUpdateMessage OrderBookUpdatePayload
  | ConnectionMessage ConnectionPayload
  | SubscriptionMessage SubscriptionPayload
  | HeartbeatMessage
  deriving (Show)

data OrderBookUpdatePayload =
  OrderBookUpdatePayload
    { krakenOrderChannelName :: !B.ByteString
    , krakenOrderSymbol      :: !B.ByteString
    , krakenOrderAsks        :: [[B.ByteString]]
    , krakenOrderBids        :: [[B.ByteString]]
    , krakenOrderTimestamp   :: !Integer
    }
  deriving (Show)

data ConnectionPayload =
  ConnectionPayload
    { krakenConnectionId     :: !Integer
    , krakenConnectionEvent  :: !B.ByteString
    , krakenConnectionStatus :: !B.ByteString
    , krakenConnectionVs     :: !B.ByteString
    }
  deriving (Show)

data SubscriptionPayload =
  SubscriptionPayload
    { krakenSubscriptionChId    :: !Integer
    , krakenSubscriptionChName  :: !B.ByteString
    , krakenSubscriptionChEvent :: !B.ByteString
    , krakenSubscriptionChPair  :: !B.ByteString
    , krakenSubscriptionStatus  :: !B.ByteString
    }
  deriving (Show)

mapByteStringToDouble :: [[B.ByteString]] -> [[Double]]
mapByteStringToDouble bs = map (map byteStringToDouble) bs

-- reduces the two different ways of how kraken sends order updates
generalizeArray :: Value -> Value
generalizeArray (Array array) =
  case V.toList array of
    [subId, Object asks, Object bids, channel, symbol] ->
      Array (V.fromList [subId, Object (HML.union asks bids), channel, symbol])
    _ -> Array array

instance FromJSON SubscriptionPayload where
  parseJSON (Object object) =
    SubscriptionPayload <$> object .: "channelID" <*> fmap textToStrict (object .: "channelName") <*>
    fmap textToStrict (object .: "event") <*>
    fmap textToStrict (object .: "pair") <*>
    fmap textToStrict (object .: "status")

instance FromJSON ConnectionPayload where
  parseJSON (Object object) =
    ConnectionPayload <$> object .: "connectionID" <*> fmap textToStrict (object .: "event") <*>
    fmap textToStrict (object .: "status") <*>
    fmap textToStrict (object .: "version")

instance FromJSON OrderBookUpdatePayload where
  parseJSON (Array array) = do
    [subId, dataObject, String channel, String symbol] <- parseJSON (generalizeArray (Array array)) -- todo rbnfrhnr fix pattern match failure
    asks <- getAsks dataObject
    bids <- getBids dataObject
    return $
      OrderBookUpdatePayload
        (textStrictToStrict channel)
        (textStrictToStrict symbol)
        asks
        bids
        (round (byteStringToDouble (getTimestampFromOrder asks bids) * 1000 * 1000))

instance FromJSON KrakenMessage where
  parseJSON (Object o) =
    case HML.lookup "event" o of
      Just (String "systemStatus") -> ConnectionMessage <$> parseJSON (Object o)
      Just (String "subscriptionStatus") -> SubscriptionMessage <$> parseJSON (Object o)
      Just (String "heartbeat") -> return HeartbeatMessage
      _ -> fail $ "Unknown message from Kraken\t" ++ show o
  parseJSON (Array a) = OrderBookUpdateMessage <$> parseJSON (Array a)
  parseJSON smth = fail $ "Unknown message format from Kraken:\t" ++ show smth

getAsks :: Value -> Parser [[B.ByteString]]
getAsks (Object object)
  | HML.member "as" object = fmap textArrayToStrict (object .: "as")
  | HML.member "a" object = fmap textArrayToStrict (object .: "a")
  | otherwise = return []

getBids :: Value -> Parser [[B.ByteString]]
getBids (Object object)
  | HML.member "bs" object = fmap textArrayToStrict (object .: "bs")
  | HML.member "b" object = fmap textArrayToStrict (object .: "b")
  | otherwise = return []

getTimestampFromOrder :: [[B.ByteString]] -> [[B.ByteString]] -> B.ByteString
getTimestampFromOrder ([_, _, timestamp]:xs) _  = timestamp
getTimestampFromOrder [] ([_, _, timestamp]:xs) = timestamp

instance ExchangeOrder KrakenMessage where
  toOrder (OrderBookUpdateMessage message) =
    map (\(price:(qty:xs)) -> partialBaseOrder (byteStringToDouble price) (byteStringToDouble qty) Ask) asksArray ++
    map (\(price:(qty:xs)) -> partialBaseOrder (byteStringToDouble price) (byteStringToDouble qty) Bid) bidsArray
    where
      partialBaseOrder price qty = BaseOrder Kraken currPair price qty timestamp
      timestamp = fromInteger $ krakenOrderTimestamp message
      currPair = getCurrencyPairFromMessage message
      asksArray = krakenOrderAsks message
      bidsArray = krakenOrderBids message
  toOrder msg = fail "No KrakenOrderMessage \n\t"

instance (ExchangeOrder a) => ExchangeOrder (Maybe a) where
  toOrder (Just message) = toOrder message
  toOrder Nothing        = []

mapToPriceQty :: [[B.ByteString]] -> [[Double]]
mapToPriceQty [] = []
mapToPriceQty ([price, qty, _]:xs) = mapToPriceQty xs ++ [[byteStringToDouble price, byteStringToDouble qty]]
mapToPriceQty (x:xs) = mapToPriceQty xs

getCurrencyPairFromMessage :: OrderBookUpdatePayload -> CurrencyPair
getCurrencyPairFromMessage updatePayload = getCurrencyPairFromChannel $ BC.unpack $ krakenOrderSymbol updatePayload

getCurrencyPairFromChannel :: String -> CurrencyPair
getCurrencyPairFromChannel "LTC/USD" = LTCUSD
getCurrencyPairFromChannel "BCH/USD" = BCHUSD
getCurrencyPairFromChannel "ETH/USD" = ETHUSD
getCurrencyPairFromChannel "XRP/USD" = XRPUSD
