{-# LANGUAGE Rank2Types #-}
module Control.Monad.IO.Logic.Internal
       ( LogicT (..)
       , runLogicT
       , observeT
       , observeAllT
       , observeManyT
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref.Logic.Internal (WrappedMonadIO (..))
import qualified Control.Monad.Ref.Logic.Internal as Internal
import Control.Monad.Trans.Class

newtype LogicT s m a
  = LogicT { unLogicT :: Internal.LogicT (WrappedMonadIO m) a
           }

instance Functor (LogicT s m) where
  fmap f = LogicT . fmap f . unLogicT

instance Applicative (LogicT s m) where
  pure = LogicT . pure
  f <*> a = LogicT $ unLogicT f <*> unLogicT a

instance MonadIO m => Alternative (LogicT s m) where
  empty = LogicT empty
  m <|> n = LogicT $ unLogicT m <|> unLogicT n

instance Monad (LogicT s m) where
  return = LogicT . return
  m >>= k = LogicT $ unLogicT m >>= unLogicT . k
  fail = LogicT . fail

instance MonadIO m => MonadPlus (LogicT s m) where
  mzero = LogicT mzero
  m `mplus` n = LogicT $ unLogicT m `mplus` unLogicT n

runLogicT :: MonadIO m =>
             (forall s . LogicT s m a) ->
             (a -> m r -> m r) ->
             m r ->
             m r
runLogicT m plus =
  unwrapMonadIO .
  Internal.runLogicT (unLogicT m) (\ a -> lift . plus a . unwrapMonadIO) .
  lift

observeT :: MonadIO m => (forall s . LogicT s m a) -> m a
observeT m = unwrapMonadIO . Internal.observeT $ unLogicT m

observeAllT :: MonadIO m => (forall s . LogicT s m a) -> m [a]
observeAllT m = unwrapMonadIO . Internal.observeAllT $ unLogicT m

observeManyT :: MonadIO m => Int -> (forall s . LogicT s m a) -> m [a]
observeManyT n m = unwrapMonadIO . Internal.observeManyT n $ unLogicT m
