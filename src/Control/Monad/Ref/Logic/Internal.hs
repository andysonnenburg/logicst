{-# LANGUAGE CPP, TypeFamilies #-}
module Control.Monad.Ref.Logic.Internal
       ( WrappedMonadIO (..)
       , LogicT
       , runLogicT
       , observeT
       , observeAllT
       , observeManyT
       , Ref
       , IORef
       , STRef
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
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader

import qualified Data.STRef as ST

class Monad m => MonadST m where
  type World m
  liftST :: ST (World m) a -> m a

instance MonadST (ST s) where
  type World (ST s) = s
  liftST = id
  {-# INLINE liftST #-}

instance MonadST IO where
  type World IO = RealWorld
  liftST = stToIO
  {-# INLINE liftST #-}

instance MonadST m => MonadST (ReaderT r m) where
  type World (ReaderT r m) = World m
  liftST = lift . liftST
  {-# INLINE liftST #-}

instance MonadST m => MonadST (Logic.LogicT m) where
  type World (Logic.LogicT m) = World m
  liftST = lift . liftST
  {-# INLINE liftST #-}

newtype WrappedMonadIO m a = WrapMonadIO { unwrapMonadIO :: m a }

type WrappedIO = WrappedMonadIO IO

instance Monad m => Monad (WrappedMonadIO m) where
  return = WrapMonadIO . return
  {-# INLINE return #-}
  m >>= k = WrapMonadIO $ unwrapMonadIO m >>= unwrapMonadIO . k
  {-# INLINE (>>=) #-}
  fail = WrapMonadIO . fail
  {-# INLINE fail #-}

instance MonadTrans WrappedMonadIO where
  lift = WrapMonadIO
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (WrappedMonadIO m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadIO m => MonadST (WrappedMonadIO m) where
  type World (WrappedMonadIO m) = RealWorld
  liftST = liftIO . stToIO
  {-# INLINE liftST #-}

newtype LogicT m a =
  LogicT { unLogicT :: ReaderT (Switch (World m)) (Logic.LogicT m) a
         }

type LogicST s = LogicT (ST s)

type LogicIO = LogicT WrappedIO

runLogicT :: MonadST m => LogicT m a -> (a -> m r -> m r) -> m r -> m r
runLogicT m plus zero = do
  switch <- newSwitch
  Logic.runLogicT (runReaderT (unLogicT m) switch) plus zero
{-# SPECIALIZE runLogicT :: LogicST s a ->
                            (a -> ST s r -> ST s r) ->
                            ST s r ->
                            ST s r #-}
{-# SPECIALIZE runLogicT :: LogicIO a ->
                            (a -> WrappedIO r -> WrappedIO r) ->
                            WrappedIO r ->
                            WrappedIO r #-}

observeT :: MonadST m => LogicT m a -> m a
observeT m = newSwitch >>= Logic.observeT . runReaderT (unLogicT m)
{-# SPECIALIZE observeT :: LogicST s a -> ST s a #-}
{-# SPECIALIZE observeT :: LogicIO a -> WrappedIO a #-}

observeAllT :: MonadST m => LogicT m a -> m [a]
observeAllT m = newSwitch >>= Logic.observeAllT . runReaderT (unLogicT m)
{-# SPECIALIZE observeAllT :: LogicST s a -> ST s [a] #-}
{-# SPECIALIZE observeAllT :: LogicIO a -> WrappedIO [a] #-}

observeManyT :: MonadST m => Int -> LogicT m a -> m [a]
observeManyT n m = newSwitch >>= Logic.observeManyT n . runReaderT (unLogicT m)
{-# SPECIALIZE observeManyT :: Int -> LogicST s a -> ST s [a] #-}
{-# SPECIALIZE observeManyT :: Int -> LogicIO a -> WrappedIO [a] #-}

using :: Monad m => r -> ReaderT r m a -> ReaderT r m a
using = Reader.local . const

instance Functor (LogicT m) where
  fmap f = LogicT . fmap f . unLogicT

instance Applicative (LogicT m) where
  pure = LogicT . pure
  f <*> a = LogicT $ unLogicT f <*> unLogicT a

instance MonadST m => Alternative (LogicT m) where
  empty = LogicT empty
  {-# INLINE empty #-}
  m <|> n = LogicT $ do
    r <- newSwitch
    using r (unLogicT m) <|> flipSwitch r *> unLogicT n
  {-# SPECIALIZE (<|>) :: LogicST s a -> LogicST s a -> LogicST s a #-}
  {-# SPECIALIZE (<|>) :: LogicIO a -> LogicIO a -> LogicIO a #-}

instance Monad (LogicT m) where
  return = LogicT . return
  m >>= k = LogicT $ unLogicT m >>= unLogicT . k
  fail = LogicT . fail

instance MonadST m => MonadPlus (LogicT m) where
  mzero = LogicT mzero
  {-# INLINE mzero #-}
  m `mplus` n = LogicT $ do
    r <- newSwitch
    using r (unLogicT m) `mplus` (flipSwitch r >> unLogicT n)
  {-# SPECIALIZE mplus :: LogicST s a -> LogicST s a -> LogicST s a #-}
  {-# SPECIALIZE mplus :: LogicIO a -> LogicIO a -> LogicIO a #-}

instance MonadST m => MonadST (LogicT m) where
  type World (LogicT m) = World m
  liftST = LogicT . lift . lift . liftST
  {-# INLINE liftST #-}

instance MonadST m => MonadLogic (LogicT m) where
  msplit = LogicT . fmap (fmap (fmap LogicT)) . msplit . unLogicT

ask :: LogicT m (Switch (World m))
ask = LogicT Reader.ask

type Switch s = ST.STRef s Bool

newSwitch :: MonadST m => m (Switch (World m))
newSwitch = liftST $ ST.newSTRef False
{-# INLINE newSwitch #-}

flipSwitch :: MonadST m => Switch (World m) -> m ()
flipSwitch = liftST . flip ST.writeSTRef True
{-# INLINE flipSwitch #-}

ifFlipped :: Switch s -> ST s a -> ST s a -> ST s a
ifFlipped switch t f = do
  p <- ST.readSTRef switch
  if p then t else f

newtype Ref s a = Ref (ST.STRef s (Value s a))

type STRef s = Ref s

type IORef = Ref RealWorld

data Value s a
  = New {-# UNPACK #-} !(Write s a)
  | {-# UNPACK #-} !(Write s a) :| !(Value s a)

data Write s a = Write {-# UNPACK #-} !(Switch s) a

newRef :: MonadST m => a -> LogicT m (Ref (World m) a)
newRef a = ask >>= liftST . fmap Ref . newSTRef a
{-# INLINE newRef #-}

newSTRef :: a -> Switch s -> ST s (ST.STRef s (Value s a))
newSTRef a = ST.newSTRef .! New . flip Write a

infixr 9 .!
(.!) :: (b -> c) -> (a -> b) -> a -> c
f .! g = \ a -> a `seq` f (g a)

readRef :: MonadST m => Ref (World m) a -> LogicT m a
readRef (Ref ref) = liftST $ readSTRef ref
{-# INLINE readRef #-}

readSTRef :: ST.STRef s (Value s a) -> ST s a
readSTRef ref = ST.readSTRef ref >>= \ value -> case value of
  Write switch a :| xs -> ifFlipped switch (backtrack xs) $ return a
  New (Write _ a) -> return a
  where
    backtrack xs@(Write switch a :| ys) =
      ifFlipped switch (backtrack ys) $
      ST.writeSTRef ref xs >> return a
    backtrack xs@(New (Write _ a)) =
      ST.writeSTRef ref xs >> return a

writeRef :: MonadST m => Ref (World m) a -> a -> LogicT m ()
writeRef ref a = modifyRef'' ref $ \ switch _ -> Write switch a
{-# INLINE writeRef #-}

modifyRef :: MonadST m => Ref (World m) a -> (a -> a) -> LogicT m ()
modifyRef ref f = modifyRef'' ref $ \ switch a -> Write switch $ f a
{-# INLINE modifyRef #-}

modifyRef' :: MonadST m => Ref (World m) a -> (a -> a) -> LogicT m ()
modifyRef' ref f = modifyRef'' ref $ \ switch a -> Write switch $! f a
{-# INLINE modifyRef' #-}

modifyRef'' :: MonadST m =>
               Ref (World m) a ->
               (Switch (World m) -> a -> Write (World m) a) -> LogicT m ()
modifyRef'' (Ref ref) f = ask >>= \ r -> liftST $ modifySTRef ref f r
{-# INLINABLE modifyRef'' #-}

modifySTRef :: ST.STRef s (Value s a) ->
               (Switch s -> a -> Write s a) ->
               Switch s ->
               ST s ()
modifySTRef ref f = \ r -> ST.readSTRef ref >>= \ value -> backtrack value r
  where
    backtrack xs@(Write switch a :| ys) r =
      ifFlipped switch
      (backtrack ys r)
      (ST.writeSTRef ref $! f r a :| if switch == r then ys else xs)
    backtrack xs@(New (Write switch a)) r =
      ST.writeSTRef ref $!
      if switch == r then New (f r a) else f r a :| xs
