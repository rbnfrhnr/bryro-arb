{-# LANGUAGE OverloadedStrings #-}

module Exchange.Binance.Contract.Websocket
  ( BinanceMessage
  ) where

import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

import           Data.Aeson
import           Data.ByteString
import           Exchange.Types
import           Exchange.Utils
import           Finance

newtype BinanceMessage =
  OrderUpdateMessage OrderUpdatePayload
  deriving (Show)

{- | represents a WebSocket message payload for an order book update -}
data OrderUpdatePayload =
  OrderUpdatePayload
    { event     :: !ByteString -- ^ name of the event. defined by binance to identify what kind of message was received
    , eventTime :: !Int -- ^ timestamp of the message
    , symbol    :: !ByteString -- ^ currencypair for which the update was received
    , bids      :: ![[ByteString]] -- ^ array which represents a tuple. fst: price, snd: qty
    , asks      :: ![[ByteString]] -- ^ array which represents a tuple. fst: price, snd: qty
    }
  deriving (Show)

instance FromJSON OrderUpdatePayload where
  parseJSON (Object object) =
    OrderUpdatePayload <$> fmap textToStrict (object .: "e") <*> object .: "E" <*> fmap textToStrict (object .: "s") <*>
    fmap textArrayToStrict (object .: "b") <*>
    fmap textArrayToStrict (object .: "a")

instance FromJSON BinanceMessage where
  parseJSON (Object v) = OrderUpdateMessage <$> parseJSON (Object v)

instance ExchangeOrder BinanceMessage where
  toOrder (OrderUpdateMessage message) =
    Prelude.map (\[price, qty] -> baseOrder (byteStringToDouble price) (byteStringToDouble qty) Ask) asksArray ++
    Prelude.map (\[price, qty] -> baseOrder (byteStringToDouble price) (byteStringToDouble qty) Bid) bidsArray
    where
      baseOrder price qty = BaseOrder Binance currencyPair price qty timestamp
      timestamp = 1000 * eventTime message
      currencyPair = toCurrencyPair $ symbol message
      asksArray = asks message
      bidsArray = bids message

instance (ExchangeOrder a) => ExchangeOrder (Maybe a) where
  toOrder (Just message) = toOrder message
  toOrder Nothing        = []

{- | maps the currencypair used by binance to our internal representation-}
toCurrencyPair :: ByteString -> CurrencyPair
toCurrencyPair "LTCUSDT" = LTCUSD
toCurrencyPair "XRPUSDT" = XRPUSD
toCurrencyPair "ETHUSDT" = ETHUSD
toCurrencyPair "BCHUSDT" = BCHUSD
