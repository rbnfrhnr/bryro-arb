{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Exchange.Network.Sync.ChanPipe (
       DeduplicationTState(..)
      ,SimpleTState(..)
      ,Transformer
      ,TransformerData(..)
      ,UpdateTransformerState
      ,WriteOut

      ,filteredChan
      ,pipe
      ,pipeChan
      ,runFilteredChan
      ,runFilteredChanAsync
      ,runPipedChan
      ,runPipedChanAsync
      ,updateTState
      ,writeOut
) where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Control.Concurrent.STM.TChan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.QSem as Sem
import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Control.Exception.Base
import Exchange.Network.Sync.Types
import Exchange.Network.Sync.Utils
import System.IO

import Exchange.Kraken.Utils as K
import Exchange.Bitstamp.Utils as BIT

data TransformerData a = TransformerData a deriving (Show)

{- | takes the TransformerData buffer a Chan and allows you to write to the output channel (not mandatory).
     returns a new TransformerData state
-}
class WriteOut t b where
    writeOut :: Chan.TChan b -> TransformerData t  -> IO (TransformerData t)

{- | Typeclass for transforming an incoming message. Acts as a buffer before writing to the output channel. -}
class UpdateTransformerState a t where
    updateTState :: TransformerData t -> a -> IO (TransformerData t)

class (UpdateTransformerState i t, WriteOut t o) => Transformer i t o where
    pipe :: i -> Chan.TChan o -> TransformerData t -> IO (TransformerData t)
    pipe input outChan tr = (updateTState tr input) >>= (writeOut outChan)


{- | Pipes messages from a source to a sink and transforms the data in-between -}
filteredChan :: (WriteOut t o, UpdateTransformerState i t) => Chan.TChan i -> Chan.TChan o -> TransformerData t -> IO (TransformerData t)
filteredChan source sink tdState = (atomically (Chan.readTChan source)) >>= (updateTState tdState) >>= (writeOut sink)

{- | Runs a filtered channel composite. Run it in a thread or otherwise it will block -}
runFilteredChan :: IO (TransformerData t) -> IO ()
runFilteredChan fChan = fChan >> runFilteredChan fChan

runFilteredChanAsync :: IO (TransformerData t) -> IO (ThreadId, Sem.QSem)
runFilteredChanAsync filteredChan = do
                              sem <- Sem.newQSem 0
                              tid <- forkFinally (runFilteredChan filteredChan) (\_ -> putStrLn "killing async piped chan\n" >> Sem.signalQSem sem)
                              return (tid, sem)

{- | Piping from source to sink. Blocking IO action -}
pipeChan :: Chan.TChan a -> Chan.TChan a -> IO ()
pipeChan source sink = (atomically (Chan.readTChan source)) >>= (atomically . (Chan.writeTChan sink))

{- | Start piping from source to sink. Be aware of blocking IO action -}
runPipedChan :: IO () -> IO ()
runPipedChan pipedChan = pipedChan >> runPipedChan pipedChan

{- | Run piping async with a semaphore to be able to wait for termination of thread in order to not lose any potential messages -}
runPipedChanAsync :: IO () -> IO (ThreadId, Sem.QSem)
runPipedChanAsync pipedChan = do
                              sem <- Sem.newQSem 0
                              tid <- forkFinally (runPipedChan pipedChan) (\_ -> putStrLn "killing async piped chan\n" >> Sem.signalQSem sem)
                              return (tid, sem)

{- | Simple data structure for deduplication of messages in a Chan -}
data DeduplicationTState = DeduplicationTState (Maybe BL.ByteString) (Map.Map BL.ByteString BL.ByteString) deriving (Show)

instance WriteOut DeduplicationTState BL.ByteString where
    writeOut chan (TransformerData (DeduplicationTState (Just msg) lookupMap)) = atomically (Chan.writeTChan chan msg) >> return (TransformerData $ DeduplicationTState Nothing (Map.insert msg msg lookupMap))
    writeOut chan (TransformerData (DeduplicationTState _ lookupMap))          = return $ TransformerData $ DeduplicationTState Nothing lookupMap

instance UpdateTransformerState BL.ByteString DeduplicationTState where
    updateTState (TransformerData (DeduplicationTState _ lookupMap)) bs = case Map.lookup bs lookupMap of
                                                                   (Just red) -> putStrLn ("PURGING REDUNDANT MESSAGE:\t" ++ (show red)) >> return (TransformerData $ DeduplicationTState Nothing lookupMap)
                                                                   Nothing    -> return $ TransformerData $ DeduplicationTState (Just bs) lookupMap

instance Transformer BL.ByteString DeduplicationTState BL.ByteString


data SimpleTState = SimpleTState (Maybe BL.ByteString) deriving (Show)

instance UpdateTransformerState BL.ByteString SimpleTState where
    updateTState (TransformerData (SimpleTState _)) newBs = return (TransformerData (SimpleTState (Just newBs)))

instance WriteOut SimpleTState BL.ByteString where
    writeOut chan (TransformerData (SimpleTState (Just msg))) = atomically (Chan.writeTChan chan msg) >> return (TransformerData $ SimpleTState Nothing)
    writeOut chan (TransformerData (SimpleTState _))   = return $ TransformerData $ SimpleTState Nothing

instance Transformer BL.ByteString SimpleTState BL.ByteString