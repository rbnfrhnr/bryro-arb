{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Finance.Types
  ( module Currency
  , module Exchange
  , module Order
  , Exchange(..)
  , Tick(..)
  , UpdateTick
  , updateTick
  ) where

import           Data.Aeson
import           Data.Map
import           Finance.Currency as Currency
import           Finance.Exchange as Exchange
import           Finance.Order    as Order
import           GHC.Generics

{- | Representation of a tick. e.g current lowest ask and highest bid orders -}
data Tick =
  Tick
    { timestamp :: !Int
    , tickAsk   :: !(Maybe Order)
    , tickBid   :: !(Maybe Order)
    }
  deriving (Show, Generic)

class UpdateTick a where
  updateTick :: a -> Tick -> Tick

instance FromJSON Tick

instance ToJSON Tick
