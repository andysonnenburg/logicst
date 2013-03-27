module Data.STRef.Logic
       ( STRef
       , newSTRef
       , readSTRef
       , writeSTRef
       , modifySTRef
       , modifySTRef'
       ) where

import Control.Monad.ST.Logic.Internal
import qualified Control.Monad.Ref.Logic.Internal as Internal

newtype STRef s a = STRef { unSTRef :: Internal.STRef s a }

newSTRef :: a -> LogicST s (STRef s a)
newSTRef = LogicST . fmap STRef . Internal.newRef
{-# INLINE newSTRef #-}

readSTRef :: STRef s a -> LogicST s a
readSTRef = LogicST . Internal.readRef . unSTRef
{-# INLINE readSTRef #-}

writeSTRef :: STRef s a -> a -> LogicST s ()
writeSTRef ref = LogicST . Internal.writeRef (unSTRef ref)
{-# INLINE writeSTRef #-}

modifySTRef :: STRef s a -> (a -> a) -> LogicST s ()
modifySTRef ref = LogicST . Internal.modifyRef (unSTRef ref)
{-# INLINE modifySTRef #-}

modifySTRef' :: STRef s a -> (a -> a) -> LogicST s ()
modifySTRef' ref = LogicST . Internal.modifyRef' (unSTRef ref)
{-# INLINE modifySTRef' #-}
