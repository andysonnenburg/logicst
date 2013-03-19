{-# LANGUAGE Rank2Types #-}
module Control.Monad.ST.Logic
       ( LogicST
       , runLogicST
       , liftST
       , STRef
       , newSTRef
       , readSTRef
       , writeSTRef
       , modifySTRef
       , modifySTRef'
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

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.STRef as ST

import Prelude hiding (dropWhile)

newtype LogicST s a =
  LogicST { unLogicST :: StateT (NonEmpty (Label s)) (LogicT (LabelSupplyST s)) a
          }

runLogicST :: (forall s . LogicST s a) -> (a -> r -> r) -> r -> r
runLogicST m plus zero = runLabelSupplyST $ do
  x <- Label.newLabel
  runLogicT (evalStateT (unLogicST m) (x :| [])) (liftM . plus) (return zero)

liftST :: ST s a -> LogicST s a
liftST = LogicST . lift . lift . Label.liftST

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

newSTRef :: a -> LogicST s (STRef s a)
newSTRef a = do
  x <- gets NonEmpty.head
  liftST $ STRef <$> ST.newSTRef (Write x a :| [])

readSTRef :: STRef s a -> LogicST s a
readSTRef ref = do
  backtrack ref
  liftST $ fromValue <$> ST.readSTRef (unSTRef ref)

writeSTRef :: STRef s a -> a -> LogicST s ()
writeSTRef ref a = do
  backtrack ref
  x <- gets NonEmpty.head
  liftST $ ST.modifySTRef (unSTRef ref) (modifyValue x a)

modifySTRef :: STRef s a -> (a -> a) -> LogicST s ()
modifySTRef ref f = writeSTRef ref . f =<< readSTRef ref

modifySTRef' :: STRef s a -> (a -> a) -> LogicST s ()
modifySTRef' ref f = do
  x <- readSTRef ref
  let x' = f x
  x' `seq` writeSTRef ref x'

label :: LogicST s ()
label = do
  x <- newLabel
  modify $ NonEmpty.cons x

backtrack :: STRef s a -> LogicST s ()
backtrack ref = do
  ws@(Write x _ :| _) <- liftST $ ST.readSTRef (unSTRef ref)
  y <- gets $ findLE x
  whenJust (dropWhile (\ (Write x' _) -> x' > y) ws) $ \ ws' ->
    liftST $ ST.writeSTRef (unSTRef ref) $! NonEmpty.fromList ws'

newLabel :: LogicST s (Label s)
newLabel = LogicST $ lift $ lift Label.newLabel

fromValue :: Value s a -> a
fromValue (Write _ a :| _) = a

modifyValue :: Label s -> a -> Value s a -> Value s a
modifyValue x a (w@(Write y _) :| ws)
  | x == y = Write x a :| ws
  | otherwise = Write x a :| w : ws

gets :: (NonEmpty (Label s) -> a) -> LogicST s a
gets = LogicST . State.gets

modify :: (NonEmpty (Label s) -> NonEmpty (Label s)) -> LogicST s ()
modify = LogicST . State.modify

dropWhile :: (a -> Bool) -> NonEmpty a -> Maybe [a]
dropWhile = (. NonEmpty.toList) . go
  where
    go _ [] = Nothing
    go p (x : xs')
      | p x = Just $ go' p xs'
      | otherwise = Nothing
    go' _ [] = []
    go' p xs@(x : xs')
      | p x = go' p xs'
      | otherwise = xs

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust p k = maybe (return ()) k p

findLE :: Ord a => a -> NonEmpty a -> a
findLE = (. NonEmpty.toList) . go
  where
    go _ [] = error "findLE: empty list"
    go y (x : xs)
      | x <= y = x
      | otherwise = go y xs
