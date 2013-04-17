{-# LANGUAGE CPP, Rank2Types, TypeFamilies #-}
module Control.Monad.ST.Logic.Internal
       ( LogicT
       , runLogicT
       , observeT
       , observeAllT
       , observeManyT
       , liftST
       , runLogicST
       , observeST
       , observeAllST
       , observeManyST
       , Ref
       , newRef
       , readRef
       , writeRef
       , modifyRef
       , modifyRef'
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Logic as Logic
import Control.Monad.Logic.Class
#ifdef MODULE_Control_Monad_ST_Safe
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import qualified Control.Monad.Trans.State.Strict as State

import qualified Data.STRef as ST

class Monad m => MonadST m where
  type World m
  liftST :: ST (World m) a -> m a

instance MonadST (ST s) where
  type World (ST s) = s
  liftST = id

instance MonadST IO where
  type World IO = RealWorld
  liftST = stToIO

newtype LogicT s m a =
  LogicT { unLogicT :: StateT (Switch m) (Logic.LogicT m) a
         }

runLogicT :: MonadST m => (forall s . LogicT s m a) -> (a -> m r -> m r) -> m r -> m r
runLogicT m = unsafeRunLogicT m
{-# INLINE runLogicT #-}

runLogicST :: (forall s . LogicT s (ST s) a) -> (a -> r -> r) -> r -> r
runLogicST m next zero = runST $ unsafeRunLogicT m (liftM . next) (return zero)
{-# INLINE runLogicST #-}

unsafeRunLogicT :: MonadST m => LogicT s m a -> (a -> m r -> m r) -> m r -> m r
unsafeRunLogicT m next zero = do
  s <- newSwitch
  Logic.runLogicT (evalStateT (unLogicT m) s) next zero
{-# SPECIALIZE unsafeRunLogicT :: LogicT s (ST s) a -> (a -> ST s r -> ST s r) -> ST s r -> ST s r #-}
{-# SPECIALIZE unsafeRunLogicT :: LogicT s IO a -> (a -> IO r -> IO r) -> IO r -> IO r #-}

observeT :: MonadST m => (forall s . LogicT s m a) -> m a
observeT m = unsafeObserveT m
{-# INLINE observeT #-}

observeST :: (forall s . LogicT s (ST s) a) -> a
observeST m = runST $ unsafeObserveT m
{-# INLINE observeST #-}

unsafeObserveT :: MonadST m => LogicT s m a -> m a
unsafeObserveT m = do
  s <- newSwitch
  Logic.observeT (evalStateT (unLogicT m) s)
{-# SPECIALIZE unsafeObserveT :: LogicT s (ST s) a -> ST s a #-}
{-# SPECIALIZE unsafeObserveT :: LogicT s IO a -> IO a #-}

observeAllT :: MonadST m => (forall s . LogicT s m a) -> m [a]
observeAllT m = unsafeObserveAllT m
{-# INLINE observeAllT #-}

observeAllST :: (forall s . LogicT s (ST s) a) -> [a]
observeAllST m = runST $ unsafeObserveAllT m
{-# INLINE observeAllST #-}

unsafeObserveAllT :: MonadST m => LogicT s m a -> m [a]
unsafeObserveAllT m = do
  s <- newSwitch
  Logic.observeAllT (evalStateT (unLogicT m) s)
{-# SPECIALIZE unsafeObserveAllT :: LogicT s (ST s) a -> ST s [a] #-}
{-# SPECIALIZE unsafeObserveAllT :: LogicT s IO a -> IO [a] #-}

observeManyT :: MonadST m => Int -> (forall s . LogicT s m a) -> m [a]
observeManyT n m = unsafeObserveManyT n m
{-# INLINE observeManyT #-}

observeManyST :: Int -> (forall s . LogicT s (ST s) a) -> [a]
observeManyST n m = runST $ unsafeObserveManyT n m
{-# INLINE observeManyST #-}

unsafeObserveManyT :: MonadST m => Int -> LogicT s m a -> m [a]
unsafeObserveManyT n m = do
  s <- newSwitch
  Logic.observeManyT n (evalStateT (unLogicT m) s)
{-# SPECIALIZE unsafeObserveManyT :: Int -> LogicT s (ST s) a -> ST s [a] #-}
{-# SPECIALIZE unsafeObserveManyT :: Int -> LogicT s IO a -> IO [a] #-}

instance Functor (LogicT s m) where
  fmap f = LogicT . fmap f . unLogicT
  {-# INLINE fmap #-}

instance Applicative (LogicT s m) where
  pure = LogicT . pure
  {-# INLINE pure #-}
  f <*> a = LogicT $ unLogicT f <*> unLogicT a
  {-# INLINE (<*>) #-}
#ifndef CLASS_OldApplicative
  a *> b = LogicT $ unLogicT a *> unLogicT b
  {-# INLINE (*>) #-}
  a <* b = LogicT $ unLogicT a <* unLogicT b
  {-# INLINE (<*) #-}
#endif

instance MonadST m => Alternative (LogicT s m) where
  empty = LogicT empty
  {-# INLINE empty #-}
  (<|>) = plusLogic
  {-# INLINE (<|>) #-}

instance Monad (LogicT s m) where
  return = LogicT . return
  {-# INLINE return #-}
  m >>= k = LogicT $ unLogicT m >>= unLogicT . k
  {-# INLINE (>>=) #-}
  m >> n = LogicT $ unLogicT m >> unLogicT n
  {-# INLINE (>>) #-}
  fail = LogicT . fail
  {-# INLINE fail #-}

instance MonadST m => MonadPlus (LogicT s m) where
  mzero = LogicT mzero
  {-# INLINE mzero #-}
  mplus = plusLogic
  {-# INLINE mplus #-}

plusLogic :: MonadST m => LogicT s m a -> LogicT s m a -> LogicT s m a
plusLogic m n = do
  s <- newSwitch
  LogicT $ unLogicT (put s *> m) <|> unLogicT (flipSwitch s *> n)
{-# SPECIALIZE plusLogic :: LogicT s (ST s) a -> LogicT s (ST s) a -> LogicT s (ST s) a #-}
{-# SPECIALIZE plusLogic :: LogicT s IO a -> LogicT s IO a -> LogicT s IO a #-}

instance MonadST m => MonadLogic (LogicT s m) where
  msplit = LogicT . fmap (fmap (fmap LogicT)) . msplit . unLogicT
  {-# INLINE msplit #-}

liftLogic :: Monad m => m a -> LogicT s m a
liftLogic = LogicT . lift . lift
{-# SPECIALIZE liftLogic :: ST s a -> LogicT s (ST s) a #-}
{-# SPECIALIZE liftLogic :: IO a -> LogicT s IO a #-}

instance MonadIO m => MonadIO (LogicT s m) where
  liftIO = liftLogic . liftIO

instance MonadST m => MonadST (LogicT s m) where
  type World (LogicT s m) = World m
  liftST = liftLogic . liftST
  {-# INLINE liftST #-}

get :: Monad m => LogicT s m (Switch m)
get = LogicT State.get
{-# SPECIALIZE get :: LogicT s (ST s) (Switch (ST s)) #-}
{-# SPECIALIZE get :: LogicT s IO (Switch IO) #-}

put :: Monad m => Switch m -> LogicT s m ()
put s = s `seq` LogicT (State.put s)
{-# SPECIALIZE put :: Switch (ST s) -> LogicT s (ST s) () #-}
{-# SPECIALIZE put :: Switch IO -> LogicT s IO () #-}

type Switch m = ST.STRef (World m) Bool

newSwitch :: MonadST m => m (Switch m)
newSwitch = liftST $ ST.newSTRef False
{-# INLINE newSwitch #-}

flipSwitch :: MonadST m => Switch m -> m ()
flipSwitch = liftST . flip ST.writeSTRef True
{-# INLINE flipSwitch #-}

ifFlipped :: Switch (ST s) -> ST s a -> ST s a -> ST s a
ifFlipped switch t f = do
  p <- ST.readSTRef switch
  if p then t else f

newtype Ref s m a = Ref (ST.STRef (World m) (Value m a))

data Value m a
  = New {-# UNPACK #-} !(Write m a)
  | {-# UNPACK #-} !(Write m a) :| !(Value m a)

data Write m a = Write {-# UNPACK #-} !(Switch m) a

newRef :: MonadST m => a -> LogicT s m (Ref s m a)
newRef a = get >>= liftST . fmap Ref . newSTRef a
{-# SPECIALIZE newRef :: a -> LogicT s (ST s) (Ref s (ST s) a) #-}
{-# SPECIALIZE newRef :: a -> LogicT s IO (Ref s IO a) #-}

newSTRef :: a -> Switch m -> ST (World m) (ST.STRef (World m) (Value m a))
newSTRef a = ST.newSTRef .! New . flip Write a

infixr 9 .!
(.!) :: (b -> c) -> (a -> b) -> a -> c
f .! g = \ a -> a `seq` f (g a)

readRef :: MonadST m => Ref s m a -> LogicT s m a
readRef (Ref ref) = liftST $ readSTRef ref
{-# SPECIALIZE readRef :: Ref s (ST s) a -> LogicT s (ST s) a #-}
{-# SPECIALIZE readRef :: Ref s IO a -> LogicT s IO a #-}

readSTRef :: ST.STRef (World m) (Value m a) -> ST (World m) a
readSTRef ref = ST.readSTRef ref >>= \ value -> case value of
  Write switch a :| xs -> ifFlipped switch (backtrack xs) $ return a
  New (Write _ a) -> return a
  where
    backtrack xs@(Write switch a :| ys) =
      ifFlipped switch (backtrack ys) $
      ST.writeSTRef ref xs >> return a
    backtrack xs@(New (Write _ a)) =
      ST.writeSTRef ref xs >> return a

writeRef :: MonadST m => Ref s m a -> a -> LogicT s m ()
writeRef ref a = modifyRef'' ref $ \ switch _ -> Write switch a
{-# SPECIALIZE writeRef :: Ref s (ST s) a -> a -> LogicT s (ST s) () #-}
{-# SPECIALIZE writeRef :: Ref s IO a -> a -> LogicT s IO () #-}

modifyRef :: MonadST m => Ref s m a -> (a -> a) -> LogicT s m ()
modifyRef ref f = modifyRef'' ref $ \ switch a -> Write switch $ f a
{-# SPECIALIZE modifyRef :: Ref s (ST s) a -> (a -> a) -> LogicT s (ST s) () #-}
{-# SPECIALIZE modifyRef :: Ref s IO a -> (a -> a) -> LogicT s IO () #-}

modifyRef' :: MonadST m => Ref s m a -> (a -> a) -> LogicT s m ()
modifyRef' ref f = modifyRef'' ref $ \ switch a -> Write switch $! f a
{-# SPECIALIZE modifyRef' :: Ref s (ST s) a -> (a -> a) -> LogicT s (ST s) () #-}
{-# SPECIALIZE modifyRef' :: Ref s IO a -> (a -> a) -> LogicT s IO () #-}

modifyRef'' :: MonadST m => Ref s m a -> (Switch m -> a -> Write m a) -> LogicT s m ()
modifyRef'' (Ref ref) f = get >>= \ r -> liftST $ modifySTRef ref f r
{-# INLINE modifyRef'' #-}

modifySTRef :: ST.STRef (World m) (Value m a) ->
               (Switch m -> a -> Write m a) ->
               Switch m ->
               ST (World m) ()
modifySTRef ref f = \ r -> ST.readSTRef ref >>= \ value -> backtrack value r
  where
    backtrack xs@(Write switch a :| ys) r =
      ifFlipped switch
      (backtrack ys r)
      (ST.writeSTRef ref $! f r a :| if switch == r then ys else xs)
    backtrack xs@(New (Write switch a)) r =
      ST.writeSTRef ref $!
      if switch == r then New (f r a) else f r a :| xs
{-# INLINE modifySTRef #-}
