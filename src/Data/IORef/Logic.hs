module Data.IORef.Logic
       ( IORef
       , newIORef
       , readIORef
       , writeIORef
       , modifyIORef
       , modifyIORef'
       ) where

import Control.Monad.IO.Logic
import Control.Monad.ST.Logic.Internal

type IORef s = Ref s IO

newIORef :: a -> LogicIO s (IORef s a)
newIORef = newRef
{-# INLINE newIORef #-}

readIORef ::IORef s a -> LogicIO s a
readIORef = readRef
{-# INLINE readIORef #-}

writeIORef :: IORef s a -> a -> LogicIO s ()
writeIORef = writeRef
{-# INLINE writeIORef #-}

modifyIORef :: IORef s a -> (a -> a) -> LogicIO s ()
modifyIORef = modifyRef
{-# INLINE modifyIORef #-}

modifyIORef' :: IORef s a -> (a -> a) -> LogicIO s ()
modifyIORef' = modifyRef'
{-# INLINE modifyIORef' #-}
