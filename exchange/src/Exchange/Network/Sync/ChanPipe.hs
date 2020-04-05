{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Exchange.Network.Sync.ChanPipe (
       DeduplicationTState(..)
      ,SimpleTState(..)
      ,Transformer
      ,TransformerData(..)
      ,UpdateTransformerState
      ,WriteOut

      ,pipe
      ,pipeChan
      ,runPipedChan
      ,runPipedChanAsync
      ,runTransformedChan
      ,runTransformedChanAsync
      ,transformedChan
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

{- | Can be used as a short time memory. Messages can be sent or not based on maybe -}
data SimpleTState a = SimpleTState (Maybe a) deriving (Show)

{- | Simple data structure for deduplication of messages in a Chan. (Long term memory based on a map)-}
data DeduplicationTState a = (Ord a, Show a) => DeduplicationTState (Maybe a) (Map.Map a a)

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


transformedChan :: (Transformer i t o) => Chan.TChan i -> Chan.TChan o -> TransformerData t -> IO (TransformerData t)
transformedChan source sink tState = (atomically (Chan.readTChan source)) >>= (\msg -> pipe msg sink tState)

runTransformedChan :: IO (TransformerData t) -> IO ()
runTransformedChan transformedChan = transformedChan >> runTransformedChan transformedChan

runTransformedChanAsync :: IO (TransformerData t) -> IO (ThreadId, Sem.QSem)
runTransformedChanAsync transformedChan = do
                            sem <- Sem.newQSem 0
                            tid <- forkFinally (runTransformedChan transformedChan) (\ _ -> Sem.signalQSem sem)
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

{- Deduplication Instances -}
instance WriteOut (DeduplicationTState a) a where
    writeOut chan (TransformerData (DeduplicationTState (Just msg) lookupMap)) = atomically (Chan.writeTChan chan msg) >> return (TransformerData $ DeduplicationTState Nothing (Map.insert msg msg lookupMap))
    writeOut chan (TransformerData (DeduplicationTState _ lookupMap))          = return $ TransformerData $ DeduplicationTState Nothing lookupMap

instance UpdateTransformerState a (DeduplicationTState a) where
    updateTState (TransformerData (DeduplicationTState _ lookupMap)) bs = case Map.lookup bs lookupMap of
                                                                   (Just red) -> putStrLn ("PURGING REDUNDANT MESSAGE:\t" ++ (show red)) >> return (TransformerData $ DeduplicationTState Nothing lookupMap)
                                                                   Nothing    -> return $ TransformerData $ DeduplicationTState (Just bs) lookupMap

instance Transformer BL.ByteString (DeduplicationTState BL.ByteString) BL.ByteString


{- SimpleTState Instances -}
instance UpdateTransformerState a (SimpleTState a) where
    updateTState (TransformerData _) newBs = return (TransformerData (SimpleTState (Just newBs)))

instance WriteOut (SimpleTState a) a where
    writeOut chan (TransformerData (SimpleTState (Just msg))) = atomically (Chan.writeTChan chan msg) >> return (TransformerData (SimpleTState Nothing))
    writeOut chan (TransformerData (SimpleTState Nothing)) = return (TransformerData (SimpleTState Nothing))

{- no need for defining class instances if all the instances are of the same type -}
instance Transformer BL.ByteString (SimpleTState BL.ByteString) BL.ByteString
instance Transformer Int (SimpleTState Int) Int

{- needs custom instances for UpdateTState and WriteOut since not all type instances are of the same type-}
instance Transformer BL.ByteString (SimpleTState Int) BL.ByteString

instance UpdateTransformerState BL.ByteString (SimpleTState Int) where
    updateTState _ newBs = return (TransformerData (SimpleTState (Just (read (show newBs)))))

instance WriteOut (SimpleTState Int) BL.ByteString where
    writeOut chan (TransformerData (SimpleTState (Just msg))) = atomically (Chan.writeTChan chan (BL8.pack (show msg))) >> return (TransformerData (SimpleTState Nothing))
    writeOut chan (TransformerData (SimpleTState Nothing)) = return (TransformerData (SimpleTState Nothing))