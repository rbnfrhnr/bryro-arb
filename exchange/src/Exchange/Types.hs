module Exchange.Types
  ( ExchangeOrder(..)
  ) where

import           Finance.Order

class ExchangeOrder a where
  toOrder :: a -> [BaseOrder]
