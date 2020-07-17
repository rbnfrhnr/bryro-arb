module Exchange.Types
  ( ExchangeOrder(..)
  ) where

import           Finance

class ExchangeOrder a where
  toOrder :: a -> [BaseOrder]
