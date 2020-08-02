{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}

module Utils.Forward
  ( Destination(..)
  , SimpleOut(..)
  , WriteOutIO
  , writeOutIO
  ) where

import           Control.Monad
import           System.IO     (hFlush, stdout)

{- | Type class for writing to the environment. Where 'a' typically is a Handle, 'b' is serializable and in
     combination can perform some IO action (writing to stdout, kafka, db etc) -}
class WriteOutIO a b where
  writeOutIO :: a -> b -> IO a

{- | Data type for simple stdout writing -}
data SimpleOut =
  SimpleOut

{- | Destination is existentially quantified to allow for heterogeneous lists with instances of WriteOut
     Since Destination itself is an instance of WriteOut, the list of Destinations, a single Destination
     or any data type which is an instance of WriteOut can be used to write to the environment
-}
data Destination b =
  forall a. WriteOutIO a b =>
            Destination a

{- | Allow every data type which is an instance of Show to be printed to stdout -}
instance (Show a) => WriteOutIO SimpleOut a where
  writeOutIO SimpleOut payload = print payload >> hFlush stdout >> return SimpleOut

{- | writeOut can be applied to any Destination holding a value of instance WriteOut -}
instance WriteOutIO (Destination b) b where
  writeOutIO (Destination dest) payload = Destination <$> writeOutIO dest payload

{- | writeOut can be applied to a list of Destinations holding a value of instance WriteOut
  This will allow for heterogeneous lists to be treated the same way as a single Destination or any other data type
  having a WriteOut instance
-}
instance WriteOutIO [Destination b] b where
  writeOutIO destinations payload =
    foldM (\list (Destination dest) -> fmap (: list) (Destination <$> writeOutIO dest payload)) [] destinations
