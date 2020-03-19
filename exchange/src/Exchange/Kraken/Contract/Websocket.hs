{-# LANGUAGE OverloadedStrings #-}
module Exchange.Kraken.Contract.Websocket(
       KrakenMessage
) where

import Data.Aeson.Types
import Finance.Types
import Exchange.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

data KrakenMessage = KrakenOrderMessage {
     krakenOrderChannelName :: !B.ByteString
    ,krakenOrderSymbol      :: !B.ByteString
    ,krakenOrderAsks        :: [[B.ByteString]]
    ,krakenOrderBids        :: [[B.ByteString]]
    ,krakenOrderTimestamp   :: !Integer
}  | KrakenConnectionMessage {
     krakenConnectionId     :: !Integer
    ,krakenConnectionEvent  :: !B.ByteString
    ,krakenConnectionStatus :: !B.ByteString
    ,krakenConnectionVs     :: !B.ByteString
}  | KrakenSubscriptionMessage {
     krakenSubscriptionChId    :: !Integer
    ,krakenSubscriptionChName  :: !B.ByteString
    ,krakenSubscriptionChEvent :: !B.ByteString
    ,krakenSubscriptionChPair  :: !B.ByteString
    ,krakenSubscriptionStatus  :: !B.ByteString
} | KrakenHeartbeatMessage deriving (Show)


textToStrict :: (T.Text -> B.ByteString)
textToStrict = TE.encodeUtf8

textArrayToStrict :: ([[TL.Text]] -> [[B.ByteString]])
textArrayToStrict = map (map (BL.toStrict . TLE.encodeUtf8))

byteStringToDouble :: B.ByteString -> Double
byteStringToDouble bs = read (BC.unpack bs) :: Double

byteStringToInteger :: B.ByteString -> Integer
byteStringToInteger bs = read (BC.unpack bs) :: Integer

mapByteStringToDouble :: [[B.ByteString]] -> [[Double]]
mapByteStringToDouble bs = map (map byteStringToDouble) bs

instance FromJSON KrakenMessage where
         parseJSON (Object o) = case (HML.lookup "event" o) of
                                Just (String "systemStatus")       -> parseKrakenConnectionMessage (Object o)
                                Just (String "subscriptionStatus") -> parseKrakenSubscriptionMessage (Object o)
                                Just (String "heartbeat")          -> return KrakenHeartbeatMessage
                                _ -> fail $ show o
         parseJSON (Array a) = case V.toList a of
                               vec@(_:_:_:_:[])                           -> parseMessage vec
                               vec@(a:(Object asks):(Object bids):b:c:[]) -> parseMessage [a,(Object (HML.union asks bids)),b,c]
                               _                                          -> fail $ "Unknown Array message from Kraken:\t" ++ (show a)
         parseJSON smth@(_)   = fail $ "Unknown message from Kraken:\t" ++ (show smth)

parseMessage :: [Value] -> Parser KrakenMessage
parseMessage (subId:dataObject:(String channel):(String symbol):[]) = do
                                                    asks <- getAsks dataObject
                                                    bids <- getBids dataObject
                                                    return $ KrakenOrderMessage bChannel bSymbol asks bids (round ((byteStringToDouble (getTimestampFromOrder asks bids)) * 1000 * 1000))
                                                    where bChannel   = textToStrict channel
                                                          bSymbol    = textToStrict symbol

getAsks :: Value -> Parser [[B.ByteString]]
getAsks (Object object) = case (HML.lookup "as" object) of
                          Just (Array _) -> fmap (textArrayToStrict) (object .: "as")
                          Nothing        -> case (HML.lookup "a" object) of
                                            Just (Array _) -> fmap (textArrayToStrict) (object .: "a")
                                            Nothing        -> return []

getBids :: Value -> Parser [[B.ByteString]]
getBids (Object object) = case (HML.lookup "bs" object) of
                          Just (Array _) -> fmap (textArrayToStrict) (object .: "bs")
                          Nothing        -> case (HML.lookup "b" object) of
                                            Just (Array _) -> fmap (textArrayToStrict) (object .: "b")
                                            Nothing        -> return []

getTimestampFromOrder :: [[B.ByteString]] -> [[B.ByteString]] -> B.ByteString
getTimestampFromOrder ((_:_:timestamp:[]):xs) _  = timestamp
getTimestampFromOrder [] ((_:_:timestamp:[]):xs) = timestamp

parseKrakenConnectionMessage :: Value -> Parser KrakenMessage
parseKrakenConnectionMessage (Object object) = KrakenConnectionMessage
                                                <$> object .: "connectionID"
                                                <*> fmap (textToStrict) (object .: "event")
                                                <*> fmap (textToStrict) (object .: "status")
                                                <*> fmap (textToStrict) (object .: "version")

parseKrakenSubscriptionMessage :: Value -> Parser KrakenMessage
parseKrakenSubscriptionMessage (Object object) = KrakenSubscriptionMessage
                                                 <$> object .: "channelID"
                                                 <*> fmap (textToStrict) (object .: "channelName")
                                                 <*> fmap (textToStrict) (object .: "event")
                                                 <*> fmap (textToStrict) (object .: "pair")
                                                 <*> fmap (textToStrict) (object .: "status")

instance ExchangeOrder KrakenMessage where
  toOrder message@(KrakenOrderMessage _ _ _ _ _) = (map (\(price:qty:xs) -> AskOrder (mapToBaseOrder currencyPair (byteStringToDouble price) (byteStringToDouble qty) msgTimestamp)) asksArray) ++ (map (\(price:qty:xs) -> BidOrder (mapToBaseOrder currencyPair (byteStringToDouble price) (byteStringToDouble qty) msgTimestamp)) bidsArray)
                where msgTimestamp = fromInteger $ krakenOrderTimestamp message
                      currencyPair = getCurrencyPairFromMessage message
                      asksArray    = krakenOrderAsks message
                      bidsArray    = krakenOrderBids message
  toOrder _ = fail

instance (ExchangeOrder a) => ExchangeOrder (Maybe a) where
  toOrder (Just message) = toOrder message
  toOrder Nothing        = []

mapToPriceQty :: [[B.ByteString]] -> [[Double]]
mapToPriceQty [] = []
mapToPriceQty ((price:qty:_:[]):xs) = (mapToPriceQty xs) ++ [[(byteStringToDouble price), (byteStringToDouble qty)]]
mapToPriceQty (x:xs) = mapToPriceQty xs

mapToBaseOrder :: CurrencyPair -> Double -> Double -> Int -> BaseOrder
mapToBaseOrder currency price qty timestamp = BaseOrder Kraken currency price qty timestamp


getCurrencyPairFromMessage :: KrakenMessage -> CurrencyPair
getCurrencyPairFromMessage krakenMessage = getCurrencyPairFromChannel $ BC.unpack $ krakenOrderSymbol krakenMessage

getCurrencyPairFromChannel :: String -> CurrencyPair
getCurrencyPairFromChannel "LTC/USD" = LTCUSD
getCurrencyPairFromChannel "BCH/USD" = BCHUSD
getCurrencyPairFromChannel "ETH/USD" = ETHUSD
getCurrencyPairFromChannel "XRP/USD" = XRPUSD
