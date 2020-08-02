{-# LANGUAGE ExistentialQuantification #-}

module Exchange.Types
  ( ExchangeOrder(..)
  , ExchangeAdapter
  , ExchangeAdapterImpl(..)
  , WebsocketConfig(..)
  , subscribeOrderBook
  ) where

import qualified Control.Concurrent.Chan as Chan

import           Finance
import           Network.Socket          (PortNumber)
import           Utils.WebSocket

class ExchangeOrder a where
  toOrder :: a -> [BaseOrder]

class ExchangeAdapter a where
  subscribeOrderBook :: a -> ([BaseOrder] -> IO ()) -> IO ()

data ExchangeAdapterImpl =
  forall a. (ExchangeAdapter a) =>
            ExchangeAdapterImpl a

data WebsocketConfig =
  WebsocketConfig
    { websocketPort :: !PortNumber
    , websocketPath :: !Path
    , websocketHost :: !Host
    }
