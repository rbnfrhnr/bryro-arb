import           Finance.Types
import           Test.QuickCheck
import           Test.QuickCheck.Gen (choose)

main :: IO ()
main = verboseCheck checkMyTest

checkMyTest :: BaseOrder -> Bool
checkMyTest (BaseOrder exc cur price qty ts) =
  exc == Bitstamp && cur == LTCUSD && (price >= 9 && price <= 10) && (qty >= 0 && qty <= 10)

instance Arbitrary BaseOrder where
  arbitrary = BaseOrder <$> return Bitstamp <*> return LTCUSD <*> choose (9, 10) <*> choose (0, 10) <*> return 0
