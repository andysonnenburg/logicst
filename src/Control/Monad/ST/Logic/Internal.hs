{-# LANGUAGE CPP, ExistentialQuantification, Rank2Types, TypeFamilies #-}
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
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
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
  LogicT { unLogicT :: ReaderT (Env m) (Logic.LogicT m) a
         }

runLogicT :: MonadST m => (forall s . LogicT s m a) -> (a -> m r -> m r) -> m r -> m r
runLogicT m = unsafeRunLogicT m
{-# INLINE runLogicT #-}

runLogicST :: (forall s . LogicT s (ST s) a) -> (a -> r -> r) -> r -> r
runLogicST m next zero = runST $ unsafeRunLogicT m (liftM . next) (return zero)
{-# INLINE runLogicST #-}

unsafeRunLogicT :: MonadST m => LogicT s m a -> (a -> m r -> m r) -> m r -> m r
unsafeRunLogicT m next zero = do
  r <- liftST newEnv
  Logic.runLogicT (runReaderT (unLogicT m) r) next zero
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
  r <- liftST newEnv
  Logic.observeT (runReaderT (unLogicT m) r)
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
  r <- liftST newEnv
  Logic.observeAllT (runReaderT (unLogicT m) r)
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
  r <- liftST newEnv
  Logic.observeManyT n (runReaderT (unLogicT m) r)
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
plusLogic m n = LogicT $ unLogicT (mark *> m) <|> unLogicT (backtrack *> n)
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

data Env m =
  Env
  {-# UNPACK #-} !(ST.STRef (World m) Mark)
  {-# UNPACK #-} !(ST.STRef (World m) (Trail m))

newEnv :: ST (World m) (Env m)
newEnv = Env <$> ST.newSTRef minBound <*> ST.newSTRef Nil

mark :: MonadST m => LogicT s m ()
mark = do
  Env m trail <- LogicT Reader.ask
  liftST $ do
    ST.modifySTRef' m (+ 1)
    ST.modifySTRef' trail Mark

backtrack :: MonadST m => LogicT s m ()
backtrack = do
  Env ref trail <- LogicT Reader.ask
  liftST $ do
    m <- ST.readSTRef ref
    ST.writeSTRef ref $! m - 1
    ST.writeSTRef trail =<< backtrackST =<< ST.readSTRef trail

backtrackST :: Trail m -> ST (World m) (Trail m)
backtrackST (Write ref m a xs) = do
  ST.writeSTRef ref $! Value m a
  backtrackST xs
backtrackST (Mark xs) =
  return xs
backtrackST Nil =
  return Nil

data Trail m
  = forall a . Write
    {-# UNPACK #-} !(ST.STRef (World m) (Value a))
    {-# UNPACK #-} !Mark
    a
    !(Trail m)
  | Mark !(Trail m)
  | Nil

addWrite :: MonadST m => ST.STRef (World m) (Value a) -> Mark -> a -> LogicT s m ()
addWrite ref m a = do
  Env _ trail <- LogicT Reader.ask
  liftST $ ST.modifySTRef' trail (Write ref m a)

newtype Ref s m a = Ref (ST.STRef (World m) (Value a)) deriving Eq

data Value a = Value {-# UNPACK #-} !Mark a

type Mark = Int

getMark :: MonadST m => LogicT s m Mark
getMark = do
  Env ref _ <- LogicT Reader.ask
  liftST $ ST.readSTRef ref

infixr 9 .!
(.!) :: (b -> c) -> (a -> b) -> a -> c
f .! g = \ a -> a `seq` f (g a)

newRef :: MonadST m => a -> LogicT s m (Ref s m a)
newRef a = getMark >>= liftST . fmap Ref . ST.newSTRef .! flip Value a
{-# SPECIALIZE newRef :: a -> LogicT s (ST s) (Ref s (ST s) a) #-}
{-# SPECIALIZE newRef :: a -> LogicT s IO (Ref s IO a) #-}

readRef :: MonadST m => Ref s m a -> LogicT s m a
readRef (Ref ref) = liftST $ do
  Value _ a <- ST.readSTRef ref
  return a
{-# SPECIALIZE readRef :: Ref s (ST s) a -> LogicT s (ST s) a #-}
{-# SPECIALIZE readRef :: Ref s IO a -> LogicT s IO a #-}

writeRef :: MonadST m => Ref s m a -> a -> LogicT s m ()
writeRef ref a = modifyRef'' ref $ \ m _ -> Value m a
{-# SPECIALIZE writeRef :: Ref s (ST s) a -> a -> LogicT s (ST s) () #-}
{-# SPECIALIZE writeRef :: Ref s IO a -> a -> LogicT s IO () #-}

modifyRef :: MonadST m => Ref s m a -> (a -> a) -> LogicT s m ()
modifyRef ref f = modifyRef'' ref $ \ m a -> Value m $ f a
{-# SPECIALIZE modifyRef :: Ref s (ST s) a -> (a -> a) -> LogicT s (ST s) () #-}
{-# SPECIALIZE modifyRef :: Ref s IO a -> (a -> a) -> LogicT s IO () #-}

modifyRef' :: MonadST m => Ref s m a -> (a -> a) -> LogicT s m ()
modifyRef' ref f = modifyRef'' ref $ \ m a -> Value m $! f a
{-# SPECIALIZE modifyRef' :: Ref s (ST s) a -> (a -> a) -> LogicT s (ST s) () #-}
{-# SPECIALIZE modifyRef' :: Ref s IO a -> (a -> a) -> LogicT s IO () #-}

modifyRef'' :: MonadST m => Ref s m a -> (Mark -> a -> Value a) -> LogicT s m ()
modifyRef'' (Ref ref) f = do
  m <- getMark
  Value m' a <- liftST $ ST.readSTRef ref
  when (m' < m) $ addWrite ref m' a
  liftST $ ST.writeSTRef ref $! f m a
{-# SPECIALIZE modifyRef'' :: Ref s (ST s) a -> (Mark -> a -> Value a) -> LogicT s (ST s) () #-}
{-# SPECIALIZE modifyRef'' :: Ref s IO a -> (Mark -> a -> Value a) -> LogicT s IO () #-}
