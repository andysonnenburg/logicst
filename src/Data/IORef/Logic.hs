module Data.IORef.Logic
       ( IORef
       , newIORef
       , readIORef
       , writeIORef
       , modifyIORef
       , modifyIORef'
       ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Logic.Internal
import qualified Control.Monad.Ref.Logic.Internal as Internal

newtype IORef s a = IORef { unIORef :: Internal.IORef a }

newIORef :: MonadIO m => a -> LogicT s m (IORef s a)
newIORef = LogicT . fmap IORef . Internal.newRef

readIORef :: MonadIO m => IORef s a -> LogicT s m a
readIORef = LogicT . Internal.readRef . unIORef

writeIORef :: MonadIO m => IORef s a -> a -> LogicT s m ()
writeIORef ref = LogicT . Internal.writeRef (unIORef ref)

modifyIORef :: MonadIO m => IORef s a -> (a -> a) -> LogicT s m ()
modifyIORef ref = LogicT . Internal.modifyRef (unIORef ref)

modifyIORef' :: MonadIO m => IORef s a -> (a -> a) -> LogicT s m ()
modifyIORef' ref = LogicT . Internal.modifyRef' (unIORef ref)
