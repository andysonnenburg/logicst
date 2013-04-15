{-# LANGUAGE CPP, Rank2Types #-}
module Control.Monad.IO.Logic
       ( LogicIO
       , runLogicIO
       , observeIO
       , observeAllIO
       , observeManyIO
       , liftST
       ) where

#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif
import Control.Monad.ST.Logic.Internal hiding (liftST)
import qualified Control.Monad.ST.Logic.Internal as Internal

runLogicIO :: (forall s . LogicIO s a) -> (a -> IO r -> IO r) -> IO r -> IO r
runLogicIO = runLogicT
{-# INLINE runLogicIO #-}

observeIO :: (forall s . LogicIO s a) -> IO a
observeIO = observeT
{-# INLINE observeIO #-}

observeAllIO :: (forall s . LogicIO s a) -> IO [a]
observeAllIO = observeAllT
{-# INLINE observeAllIO #-}

observeManyIO :: Int -> (forall s . LogicIO s a) -> IO [a]
observeManyIO = observeManyT
{-# INLINE observeManyIO #-}

liftST :: ST RealWorld a -> LogicIO s a
liftST = Internal.liftST
{-# INLINE liftST #-}
