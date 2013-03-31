{-# LANGUAGE CPP, Rank2Types, TypeFamilies #-}
module Control.Monad.ST.Logic.Internal
       ( LogicT
       , runLogicT
       , observeT
       , observeAllT
       , observeManyT
       , LogicST
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
#if MIN_VERSION_base(4, 4, 0)
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import qualified Control.Monad.Trans.Reader as Reader

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
  LogicT { unLogicT :: Logic.LogicT (ReaderT (Switch m) m) a
         }

type LogicST s = LogicT s (ST s)

runLogicT :: MonadST m => (forall s . LogicT s m a) -> (a -> m r -> m r) -> m r -> m r
runLogicT m = runLogicT' m
{-# INLINE runLogicT #-}

runLogicST :: (forall s . LogicST s a) -> (a -> r -> r) -> r -> r
runLogicST m next zero = runST $ runLogicT' m (liftM . next) (return zero)

runLogicT' :: MonadST m => LogicT s m a -> (a -> m r -> m r) -> m r -> m r
runLogicT' m next zero =
  newSwitch >>= runReaderT (Logic.runLogicT (unLogicT m) next' zero')
  where
    next' a r = ReaderT $ next a . runReaderT r
    zero' = lift zero
{-# INLINABLE runLogicT' #-}

observeT :: MonadST m => (forall s . LogicT s m a) -> m a
observeT m = observeT' m
{-# INLINE observeT #-}

observeST :: (forall s . LogicST s a) -> a
observeST m = runST $ observeT' m

observeT' :: MonadST m => LogicT s m a -> m a
observeT' m = newSwitch >>= runReaderT (Logic.observeT (unLogicT m))
{-# INLINABLE observeT' #-}

observeAllT :: MonadST m => (forall s . LogicT s m a) -> m [a]
observeAllT m = observeAllT' m
{-# INLINE observeAllT #-}

observeAllST :: (forall s . LogicST s a) -> [a]
observeAllST m = runST $ observeAllT' m

observeAllT' :: MonadST m => LogicT s m a -> m [a]
observeAllT' m = newSwitch >>= runReaderT (Logic.observeAllT (unLogicT m))
{-# INLINABLE observeAllT' #-}

observeManyT :: MonadST m => Int -> (forall s . LogicT s m a) -> m [a]
observeManyT n m = observeManyT' n m
{-# INLINE observeManyT #-}

observeManyST :: Int -> (forall s . LogicST s a) -> [a]
observeManyST n m = runST $ observeManyT' n m

observeManyT' :: MonadST m => Int -> LogicT s m a -> m [a]
observeManyT' n m = newSwitch >>= runReaderT (Logic.observeManyT n (unLogicT m))
{-# INLINABLE observeManyT' #-}

using :: Monad m => Switch m -> LogicT s m a -> LogicT s m a
using r m = LogicT $ Logic.LogicT $ \ next zero ->
   Reader.local (const r) $ Logic.unLogicT (unLogicT m) next zero

instance Functor (LogicT s m) where
  fmap f = LogicT . fmap f . unLogicT
  {-# INLINE fmap #-}

instance Applicative (LogicT s m) where
  pure = LogicT . pure
  {-# INLINE pure #-}
  f <*> a = LogicT $ unLogicT f <*> unLogicT a
  {-# INLINE (<*>) #-}

instance MonadST m => Alternative (LogicT s m) where
  empty = LogicT empty
  {-# INLINE empty #-}
  m <|> n = do
    r <- newSwitch
    LogicT $ unLogicT (using r m) <|> unLogicT (flipSwitch r *> n)
  {-# INLINABLE (<|>) #-}

instance Monad (LogicT s m) where
  return = LogicT . return
  {-# INLINE return #-}
  m >>= k = LogicT $ unLogicT m >>= unLogicT . k
  {-# INLINE (>>=) #-}
  fail = LogicT . fail
  {-# INLINE fail #-}

instance MonadST m => MonadPlus (LogicT s m) where
  mzero = LogicT mzero
  {-# INLINE mzero #-}
  m `mplus` n = do
    r <- newSwitch
    LogicT $ unLogicT (using r m) `mplus` unLogicT (flipSwitch r >> n)
  {-# INLINABLE mplus #-}

instance MonadST m => MonadLogic (LogicT s m) where
  msplit = LogicT . fmap (fmap (fmap LogicT)) . msplit . unLogicT
  {-# INLINE msplit #-}

lift' :: Monad m => m a -> LogicT s m a
lift' = LogicT . lift . lift

instance MonadIO m => MonadIO (LogicT s m) where
  liftIO = lift' . liftIO

instance MonadST m => MonadST (LogicT s m) where
  type World (LogicT s m) = World m
  liftST = lift' . liftST

ask :: Monad m => LogicT s m (Switch m)
ask = LogicT $ lift Reader.ask

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
newRef a = ask >>= liftST . fmap Ref . newSTRef a
{-# INLINABLE newRef #-}

newSTRef :: a -> Switch m -> ST (World m) (ST.STRef (World m) (Value m a))
newSTRef a = ST.newSTRef .! New . flip Write a

infixr 9 .!
(.!) :: (b -> c) -> (a -> b) -> a -> c
f .! g = \ a -> a `seq` f (g a)

readRef :: MonadST m => Ref s m a -> LogicT s m a
readRef (Ref ref) = liftST $ readSTRef ref
{-# INLINABLE readRef #-}

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
{-# INLINE writeRef #-}

modifyRef :: MonadST m => Ref s m a -> (a -> a) -> LogicT s m ()
modifyRef ref f = modifyRef'' ref $ \ switch a -> Write switch $ f a
{-# INLINE modifyRef #-}

modifyRef' :: MonadST m => Ref s m a -> (a -> a) -> LogicT s m ()
modifyRef' ref f = modifyRef'' ref $ \ switch a -> Write switch $! f a
{-# INLINE modifyRef' #-}

modifyRef'' :: MonadST m => Ref s m a -> (Switch m -> a -> Write m a) -> LogicT s m ()
modifyRef'' (Ref ref) f = ask >>= \ r -> liftST $ modifySTRef ref f r
{-# INLINABLE modifyRef'' #-}

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
