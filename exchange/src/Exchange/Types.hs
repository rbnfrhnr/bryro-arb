module Exchange.Types
  ( ExchangeOrder(..)
  ) where

import           Finance.Types

class ExchangeOrder a where
  toOrder :: a -> [BaseOrder]
