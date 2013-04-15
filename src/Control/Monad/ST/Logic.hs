{-# LANGUAGE CPP, Rank2Types #-}
module Control.Monad.ST.Logic
       ( LogicST
       , runLogicST
       , observeST
       , observeAllST
       , observeManyST
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif
import Control.Monad.ST.Logic.Internal (LogicST)
import qualified Control.Monad.ST.Logic.Internal as Internal

{-# SPECIALIZE Internal.logicPlus :: LogicST s a -> LogicST s a -> LogicST s a #-}
{-# SPECIALIZE Internal.unsafeObserveT :: LogicST s a -> ST s a #-}
{-# SPECIALIZE Internal.unsafeObserveManyT :: Int -> LogicST s a -> ST s [a] #-}
{-# SPECIALIZE Internal.unsafeObserveAllT :: LogicST s a -> ST s [a] #-}

runLogicST :: (forall s . LogicST s a) -> (a -> r -> r) -> r -> r
runLogicST = Internal.runLogicST
{-# INLINE runLogicST #-}

observeST :: (forall s . LogicST s a) -> a
observeST = Internal.observeST
{-# INLINE observeST #-}

observeAllST :: (forall s . LogicST s a) -> [a]
observeAllST = Internal.observeAllST
{-# INLINE observeAllST #-}

observeManyST :: Int -> (forall s . LogicST s a) -> [a]
observeManyST = Internal.observeManyST
{-# INLINE observeManyST #-}
