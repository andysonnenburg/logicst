{-# LANGUAGE Rank2Types #-}
module Control.Monad.ST.Label
       ( LabelSupplyST
       , runLabelSupplyST
       , Label
       , newLabel
       , liftST
       ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.ST

import Data.STRef

newtype LabelSupplyST s a
  = LabelSupplyST { unLabelSupplyST :: ReaderT (STRef s Integer) (ST s) a
                  }

runLabelSupplyST :: (forall s . LabelSupplyST s a) -> a
runLabelSupplyST m = runST $ do
  r <- newSTRef $ fromIntegral (minBound :: Int)
  runReaderT (unLabelSupplyST m) r

instance Functor (LabelSupplyST s) where
  fmap f = LabelSupplyST . fmap f . unLabelSupplyST

instance Applicative (LabelSupplyST s) where
  pure = LabelSupplyST . pure
  f <*> a = LabelSupplyST $ unLabelSupplyST f <*> unLabelSupplyST a

instance Monad (LabelSupplyST s) where
  return = LabelSupplyST . return
  m >>= k = LabelSupplyST $ unLabelSupplyST m >>= (unLabelSupplyST . k)
  m >> n = LabelSupplyST $ unLabelSupplyST m >> unLabelSupplyST n
  fail = LabelSupplyST . fail

newtype Label s = Label Integer deriving (Eq, Ord)

newLabel :: LabelSupplyST s (Label s)
newLabel = LabelSupplyST $ do
  r <- ask
  x <- lift $ readSTRef r
  lift $ writeSTRef r $! x + 1
  return $ Label x

liftST :: ST s a -> LabelSupplyST s a
liftST = LabelSupplyST . lift
