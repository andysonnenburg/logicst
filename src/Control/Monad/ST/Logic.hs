{-# LANGUAGE CPP, Rank2Types #-}
module Control.Monad.ST.Logic
       ( LogicST
       , runLogicST
       , observeST
       , observeAllST
       , observeManyST
       , liftST
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif
import Control.Monad.ST.Logic.Internal (LogicT)
import qualified Control.Monad.ST.Logic.Internal as Internal

type LogicST s = LogicT s (ST s)

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

liftST :: ST s a -> LogicST s a
liftST = Internal.liftST
{-# INLINE liftST #-}
