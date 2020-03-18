{-# LANGUAGE OverloadedStrings #-}
module Exchange.Kraken.Contract.Websocket(
       KrakenOrderMessage
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

data KrakenOrderMessage = KrakenOrderMessage {
     krakenOrderChannelName :: !B.ByteString
    ,krakenOrderSymbol      :: !B.ByteString
    ,krakenOrderAsks        :: [[Double]]
    ,krakenOrderBids        :: [[Double]]
    ,krakenOrderTimestamp   :: !Int
} deriving (Show)


textToStrict :: (T.Text -> B.ByteString)
textToStrict = TE.encodeUtf8

textArrayToStrict :: ([[TL.Text]] -> [[B.ByteString]])
textArrayToStrict = map (map (BL.toStrict . TLE.encodeUtf8))

byteStringToDouble :: B.ByteString -> Double
byteStringToDouble bs = read (BC.unpack bs) :: Double

byteStringToInteger :: B.ByteString -> Int
byteStringToInteger bs = read (BC.unpack bs) :: Int

mapByteStringOrders :: [[B.ByteString]] -> [[Double]]
mapByteStringOrders orders = map (map byteStringToDouble) orders

instance FromJSON KrakenOrderMessage where
         parseJSON (Array a) = case V.toList a of
                               vec@(subId:datObject:channel:symbol:[]) -> parseMessage vec
                               _                                       -> fail $ "Unknown message from Kraken:\t" ++ (show a)
         parseJSON smth@(_)  = fail $ "Unknown message from Kraken:\t" ++ (show smth)

parseMessage :: [Value] -> Parser KrakenOrderMessage
parseMessage (subId:dataObject:(String channel):(String symbol):[]) = do
                                                    (asks, bids) <- getOrders dataObject
--                                                    let bTimestamp = byteStringToInteger $ getTimestampFromOrder (asks, bids)
--                                                    return $ KrakenOrderMessage bChannel bSymbol (mapByteStringOrders asks) (mapByteStringOrders bids) $ bTimestamp
                                                    return $ KrakenOrderMessage bChannel bSymbol asks bids 0
                                                    where bChannel   = textToStrict channel
                                                          bSymbol    = textToStrict symbol

getOrders :: Value -> Parser ([[B.ByteString]], [[B.ByteString]])
getOrders (Object object) = case (HML.lookup "as" object) of
                            Just object -> parseInitialOrders object
                            Nothing     -> case (HML.lookup "a" object) of
                                           Just obj -> parseUpdateOrders obj
                                           _        -> fail $ show object

parseInitialOrders :: Value -> Parser ([[B.ByteString]], [[B.ByteString]])
parseInitialOrders (Object object) = do
                                     asks <- fmap (textArrayToStrict) (object .:? "as" .!= [])
                                     bids <- fmap (textArrayToStrict) (object .:? "bs" .!= [])
                                     return $ (asks, bids)
parseInitialOrders smth            = fail $ show smth

parseUpdateOrders :: Value -> Parser ([[B.ByteString]], [[B.ByteString]])
parseUpdateOrders (Object object) = do
                                    asks <- fmap (textArrayToStrict) (object .:? "a" .!= [])
                                    bids <- fmap (textArrayToStrict) (object .:? "b" .!= [])
                                    return $ (asks, bids)
parseUpdateOrders smth            = fail $ show smth

getTimestampFromOrder :: ([[B.ByteString]], [[B.ByteString]]) -> B.ByteString
getTimestampFromOrder (((_:_:timestamp:[]):xs), _) = timestamp
getTimestampFromOrder ([],((_:_:timestamp:[]):xs)) = timestamp

instance ExchangeOrder KrakenOrderMessage where
  toOrder message@(KrakenOrderMessage _ _ _ _ _) = (map (\(price:qty:xs) -> AskOrder (mapToBaseOrder currencyPair price qty msgTimestamp)) asksArray) ++ (map (\(price:qty:xs) -> BidOrder (mapToBaseOrder currencyPair price qty msgTimestamp)) bidsArray)
                where msgTimestamp = krakenOrderTimestamp message
                      currencyPair = getCurrencyPairFromMessage message
                      asksArray    = krakenOrderAsks message
                      bidsArray    = krakenOrderBids message

instance (ExchangeOrder a) => ExchangeOrder (Maybe a) where
  toOrder (Just message) = toOrder message
  toOrder Nothing        = []

mapToBaseOrder :: CurrencyPair -> Double -> Double -> Int -> BaseOrder
mapToBaseOrder currency price qty timestamp = BaseOrder Kraken currency price qty timestamp

getCurrencyPairFromMessage :: KrakenOrderMessage -> CurrencyPair
getCurrencyPairFromMessage krakenMessage = getCurrencyPairFromChannel $ BC.unpack $ krakenOrderChannelName krakenMessage

getCurrencyPairFromChannel :: String -> CurrencyPair
getCurrencyPairFromChannel "LTC/USD" = LTCUSD
getCurrencyPairFromChannel "BCH/USD" = BCHUSD
getCurrencyPairFromChannel "ETH/USD" = ETHUSD
getCurrencyPairFromChannel "XRP/USD" = XRPUSD
