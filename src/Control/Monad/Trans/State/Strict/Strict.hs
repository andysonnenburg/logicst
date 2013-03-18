module Control.Monad.Trans.State.Strict.Strict
       ( StateT
       , evalStateT
       , gets
       , modify
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

newtype StateT s m a = StateT { unStateT :: s -> m (Pair a s) }

data Pair a b = a :*: !b

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
  a :*: _ <- unStateT m s
  return a

state :: Monad m => (s -> Pair a s) -> StateT s m a
state f = StateT (return . f)

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ \ s -> fmap (\ (a :*: s') -> f a :*: s') $ unStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => Monad (StateT s m) where
  return a = state $ \ s -> a :*: s
  m >>= k = StateT $ \ s -> do
    a :*: s' <- unStateT m s
    unStateT (k a) s'
  fail str = StateT $ \ _ -> fail str

instance MonadPlus m => MonadPlus (StateT s m) where
  mzero = StateT $ const mzero
  m `mplus` n = StateT $ \ s -> unStateT m s `mplus` unStateT n s

instance MonadTrans (StateT s) where
  lift m = StateT $ \ s -> do
    a <- m
    return $ a :*: s

gets :: Monad m => (s -> a) -> StateT s m a
gets f = state $ \ s -> f s :*: s

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \ s -> () :*: f s
