{-# LANGUAGE CPP, Rank2Types #-}
module Control.Monad.ST.Logic.Internal
       ( LogicST (..)
       , runLogicST
       , observeST
       , observeAllST
       , observeManyST
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic.Class
-- #if MIN_VERSION_base(4, 4, 0)
import Control.Monad.ST.Safe
-- #else
-- import Control.Monad.ST
-- #endif
import Control.Monad.Ref.Logic.Internal

newtype LogicST s a = LogicST { unLogicST :: LogicT (ST s) a }

instance Functor (LogicST s) where
  fmap f = LogicST . fmap f . unLogicST

instance Applicative (LogicST s) where
  pure = LogicST . pure
  f <*> a = LogicST $ unLogicST f <*> unLogicST a

instance Alternative (LogicST s) where
  empty = LogicST empty
  m <|> n = LogicST $ unLogicST m <|> unLogicST n

instance Monad (LogicST s) where
  return = LogicST . return
  m >>= k = LogicST $ unLogicST m >>= unLogicST . k
  fail = LogicST . fail

instance MonadPlus (LogicST s) where
  mzero = LogicST mzero
  m `mplus` n = LogicST $ unLogicST m `mplus` unLogicST n

instance MonadLogic (LogicST s) where
  msplit = LogicST . fmap (fmap (fmap LogicST)) . msplit . unLogicST

runLogicST :: (forall s . LogicST s a) -> (a -> r -> r) -> r -> r
runLogicST m plus zero =
  runST $ runLogicT (unLogicST m) (liftM . plus) (return zero)

observeST :: (forall s . LogicST s a) -> a
observeST m = runST $ observeT $ unLogicST m

observeAllST :: (forall s . LogicST s a) -> [a]
observeAllST m = runST $ observeAllT $ unLogicST m

observeManyST :: Int -> (forall s . LogicST s a) -> [a]
observeManyST n m = runST $ observeManyT n $ unLogicST m
