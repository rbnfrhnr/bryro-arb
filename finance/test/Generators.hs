module Generators
  ( generateBaseOrder
  ) where

import           Finance
import           Test.QuickCheck
import           Test.QuickCheck.Gen (choose)

generateBaseOrder :: Exchange -> CurrencyPair -> Gen BaseOrder
generateBaseOrder exchange currency =
  BaseOrder <$> return exchange <*> return currency <*> choose (9, 10) <*> choose (0, 10) <*> return 0 <*> arbitrary

instance Arbitrary BaseOrder where
  arbitrary =
    BaseOrder <$> arbitrary <*> arbitrary <*> choose (40, 55) <*> choose (0, 10) <*> choose (1000, 20000) <*> arbitrary

instance Arbitrary OrderType where
  arbitrary = oneof [pure Ask, pure Bid]

instance Arbitrary CurrencyPair where
  arbitrary = oneof [pure LTCUSD, pure ETHUSD, pure XRPUSD, pure BCHUSD]

instance Arbitrary Exchange where
  arbitrary = oneof [pure Bitstamp, pure Binance, pure Kraken]
