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
