module Exchange
  ( createAllExchanges
  , subscribeOrderBookAll
  ) where

import qualified Control.Concurrent.Chan as Chan
import           Control.Monad           (foldM_)
import           Exchange.Binance        as Binance
import           Exchange.Bitstamp       as Bitstamp
import           Exchange.Kraken         as Kraken
import           Exchange.Types          (ExchangeAdapterImpl (..),
                                          subscribeOrderBook)
import           Finance

createAllExchanges :: [ExchangeAdapterImpl]
createAllExchanges = [ExchangeAdapterImpl Bitstamp.new, ExchangeAdapterImpl Kraken.new, ExchangeAdapterImpl Binance.new]

subscribeOrderBookAll :: Chan.Chan [BaseOrder] -> IO [ExchangeAdapterImpl]
subscribeOrderBookAll queue =
  foldM_
    (\queue (ExchangeAdapterImpl exchange) -> subscribeOrderBook exchange (Chan.writeChan queue) >> pure queue)
    queue
    exchanges >>
  pure exchanges
  where
    exchanges = createAllExchanges
