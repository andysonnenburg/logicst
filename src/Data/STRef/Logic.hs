{-# LANGUAGE CPP #-}
module Data.STRef.Logic
       ( STRef
       , newSTRef
       , readSTRef
       , writeSTRef
       , modifySTRef
       , modifySTRef'
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif
import Control.Monad.ST.Logic
import Control.Monad.ST.Logic.Internal

type STRef s = Ref s (ST s)

newSTRef :: a -> LogicST s (STRef s a)
newSTRef = newRef
{-# INLINE newSTRef #-}

readSTRef :: STRef s a -> LogicST s a
readSTRef = readRef
{-# INLINE readSTRef #-}

writeSTRef :: STRef s a -> a -> LogicST s ()
writeSTRef = writeRef
{-# INLINE writeSTRef #-}

modifySTRef :: STRef s a -> (a -> a) -> LogicST s ()
modifySTRef = modifyRef
{-# INLINE modifySTRef #-}

modifySTRef' :: STRef s a -> (a -> a) -> LogicST s ()
modifySTRef' = modifyRef'
{-# INLINE modifySTRef' #-}
