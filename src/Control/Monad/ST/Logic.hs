{-# LANGUAGE CPP, Rank2Types #-}
module Control.Monad.ST.Logic
       ( LogicST
       , runLogicST
       , observeST
       , observeAllST
       , observeManyST
       ) where

import Control.Monad.ST.Logic.Internal (LogicST)
import qualified Control.Monad.ST.Logic.Internal as Internal

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
