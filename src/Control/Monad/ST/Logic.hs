{-# LANGUAGE Rank2Types #-}
module Control.Monad.ST.Logic
       ( LogicST
       , runLogicST
       , liftST
       , STRef
       , newSTRef
       , readSTRef
       , writeSTRef
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic (LogicT, runLogicT)
import Control.Monad.ST
import Control.Monad.ST.Label (Label, LabelSupplyST, runLabelSupplyST)
import qualified Control.Monad.ST.Label as Label
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State.Strict.Strict (StateT, evalStateT)
import qualified Control.Monad.Trans.State.Strict.Strict as State

import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.STRef as ST

newtype LogicST s a =
  LogicST { unLogicST :: StateT (NonEmpty (Label s)) (LogicT (LabelSupplyST s)) a
          }

runLogicST :: (forall s . LogicST s a) -> (a -> r -> r) -> r -> r
runLogicST m plus zero = runLabelSupplyST $ do
  x <- Label.newLabel
  runLogicT (evalStateT (unLogicST m) (x :| [])) (liftM . plus) (return zero)

instance Functor (LogicST s) where
  fmap f = LogicST . fmap f . unLogicST

instance Applicative (LogicST s) where
  pure = LogicST . pure
  f <*> a = LogicST $ unLogicST f <*> unLogicST a

instance Alternative (LogicST s) where
  empty = LogicST empty
  m <|> n = LogicST $ unLogicST (label >> m) <|> unLogicST (label >> n)

instance Monad (LogicST s) where
  return = LogicST . return
  m >>= k = LogicST $ unLogicST m >>= (unLogicST . k)
  m >> n = LogicST $ unLogicST m >> unLogicST n
  fail = LogicST . fail

instance MonadPlus (LogicST s) where
  mzero = LogicST mzero
  m `mplus` n = LogicST $ unLogicST (label >> m) `mplus` unLogicST (label >> n)

newtype STRef s a = STRef { unSTRef :: ST.STRef s (Value s a) }

type Value s a = NonEmpty (Write s a)

data Write s a = Write !(Label s) a

label :: LogicST s ()
label = do
  x <- newLabel
  modify $ NonEmpty.cons x

newSTRef :: a -> LogicST s (STRef s a)
newSTRef a = do
  x <- gets NonEmpty.head
  liftST $ STRef <$> ST.newSTRef (Write x a :| [])

readSTRef :: STRef s a -> LogicST s a
readSTRef ref = do
  backtrack ref
  liftST $ fromValue <$> ST.readSTRef (unSTRef ref)

fromValue :: Value s a -> a
fromValue (Write _ a :| _) = a

writeSTRef :: STRef s a -> a -> LogicST s ()
writeSTRef ref a = do
  backtrack ref
  x <- gets NonEmpty.head
  liftST $ ST.modifySTRef (unSTRef ref) (modifyValue x a)

modifyValue :: Label s -> a -> Value s a -> Value s a
modifyValue x a (w@(Write y _) :| ws)
  | x == y = Write x a :| ws
  | otherwise = Write x a :| w : ws

backtrack :: STRef s a -> LogicST s ()
backtrack ref = do
  ws@(Write x _ :| _) <- liftST $ ST.readSTRef (unSTRef ref)
  y <- gets $ findLE x
  let ws' = NonEmpty.fromList $ NonEmpty.dropWhile (\ (Write x' _) -> x' > y) ws
  liftST $ ST.writeSTRef (unSTRef ref) ws'

class FindLE f where
  findLE :: Ord a => a -> f a -> a

instance FindLE NonEmpty where
  findLE y (x :| xs)
    | x <= y = x
    | otherwise = findLE y xs

instance FindLE [] where
  findLE _ [] = error "findLE: empty list"
  findLE y (x : xs)
    | x <= y = x
    | otherwise = findLE y xs

newLabel :: LogicST s (Label s)
newLabel = LogicST $ lift $ lift Label.newLabel

liftST :: ST s a -> LogicST s a
liftST = LogicST . lift . lift . Label.liftST

gets :: (NonEmpty (Label s) -> a) -> LogicST s a
gets = LogicST . State.gets

modify :: (NonEmpty (Label s) -> NonEmpty (Label s)) -> LogicST s ()
modify = LogicST . State.modify
