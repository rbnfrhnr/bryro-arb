{-# LANGUAGE DeriveGeneric #-}

module Finance.Tick
  ( Tick(..)
  ) where

import           Data.Aeson    (FromJSON, ToJSON)
import           Finance.Order
import           GHC.Generics  (Generic)

data Tick =
  Tick
    { tickAskOrder  :: !(Maybe Order)
    , tickBidOrder  :: !(Maybe Order)
    , tickTimestamp :: !Int
    }
  deriving (Show, Generic)

instance ToJSON Tick

instance FromJSON Tick

instance Eq Tick where
  (==) (Tick ask1 bid1 _) (Tick ask2 bid2 _) = ask1 == ask2 && bid1 == bid2
