module Exchange
  ( createAllExchanges
  ) where

import           Exchange.Binance  as Binance
import           Exchange.Bitstamp as Bitstamp
import           Exchange.Kraken   as Kraken
import           Exchange.Types    (ExchangeAdapterImpl (..))

createAllExchanges :: [ExchangeAdapterImpl]
createAllExchanges = [ExchangeAdapterImpl Bitstamp.new, ExchangeAdapterImpl Kraken.new, ExchangeAdapterImpl Binance.new]
