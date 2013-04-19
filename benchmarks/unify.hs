{-# LANGUAGE
    DefaultSignatures
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , FlexibleContexts
  , FlexibleInstances
  , NoMonomorphismRestriction
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import Control.Applicative
import Control.Monad (MonadPlus, (>=>), join, liftM, mzero)
import Control.Monad.IO.Logic
import Criterion
import Criterion.Main

import Data.Foldable
import Data.Functor.Foldable (Base, Fix (Fix), Unfoldable (embed), refix)
import Data.IORef.Logic
import Data.Traversable

import GHC.Generics

import Prelude hiding (foldr, mapM, tail)

main :: IO ()
main = defaultMain
       [ bench "zebra" $ whnfIO $ observeAllIO $ do
            houses <- freshTerm
            waterDrinker <- freshTerm
            zebraOwner <- freshTerm
            zebraProblem houses waterDrinker zebraOwner
            freeze houses
       ]

newtype IOVar s a = IOVar { unIOVar :: IORef s (Maybe a) } deriving Eq

instance IsVar (IOVar s)

instance MonadVar (LogicIO s) where
  type Var (LogicIO s) = IOVar s
  freshVar = liftM IOVar $ newIORef Nothing
  readVar = readIORef . unIOVar
  writeVar = (. Just) . writeIORef . unIOVar

data Value a
  = Nil
  | Cons a a

  | House a a a a a

  | Englishman
  | Spaniard
  | Ukrainian
  | Japanese
  | Norwegian

  | Dog
  | Snails
  | Fox
  | Horse
  | Zebra

  | Winstons
  | Kools
  | Chesterfield
  | Luckystrike
  | Parliaments

  | Milk
  | Coffee
  | Tea
  | Oj
  | Water

  | Red
  | Green
  | Ivory
  | Yellow
  | Blue deriving (Show, Eq, Functor, Foldable, Traversable, Generic1)

instance Unifiable Value

nil = pure $ embed Nil
cons = liftA2 (\ x xs -> embed $ Cons x xs)
house a b c d e = (\ a' b' c' d' e' -> embed $ House a' b' c' d' e') <$> a <*> b <*> c <*> d <*> e
englishman = pure $ embed Englishman
spaniard = pure $ embed Spaniard
ukrainian = pure $ embed Ukrainian
japanese = pure $ embed Japanese
norwegian = pure $ embed Norwegian
dog = pure $ embed Dog
snails = pure $ embed Snails
fox = pure $ embed Fox
horse = pure $ embed Horse
zebra = pure $ embed Zebra
winstons = pure $ embed Winstons
kools = pure $ embed Kools
chesterfield = pure $ embed Chesterfield
luckystrike = pure $ embed Luckystrike
parliaments = pure $ embed Parliaments
milk = pure $ embed Milk
coffee = pure $ embed Coffee
tea = pure $ embed Tea
oj = pure $ embed Oj
water = pure $ embed Water
red = pure $ embed Red
green = pure $ embed Green
ivory = pure $ embed Ivory
yellow = pure $ embed Yellow
blue = pure $ embed Blue

fromList = foldr cons nil

zebraProblem h w z = do
  (h #=) =<< fromList [house norwegian __ __ __ __, __, house __ __ __ milk __, __, __]
  join $ member <$> house englishman __ __ __ red <*> pure h
  join $ member <$> house spaniard dog __ __ __ <*> pure h
  join $ member <$> house __ __ __ coffee green <*> pure h
  join $ member <$> house ukrainian __ __ tea __ <*> pure h
  join $ iright <$> house __ __ __ __ ivory <*> house __ __ __ __ green <*> pure h
  join $ member <$> house __ snails winstons __ __ <*> pure h
  join $ member <$> house __ __ kools __ yellow <*> pure h
  join $ nextto <$> house __ __ chesterfield __ __ <*> house __ fox __ __ __ <*> pure h
  join $ nextto <$> house __ __ kools __ __ <*> house __ horse __ __ __ <*> pure h
  join $ member <$> house __ __ luckystrike oj __ <*> pure h
  join $ member <$> house japanese __ parliaments __ __ <*> pure h
  join $ nextto <$> house norwegian __ __ __ __ <*> house __ __ __ __ blue <*> pure h
  join $ member <$> house (pure w) __ __ water __ <*> pure h
  join $ member <$> house (pure z) zebra __ __ __ <*> pure h

nextto x y list =
  iright x y list
  <|>
  iright y x list

iright left right list =
  do (list #=) =<< cons (pure left) (cons (pure right) __)
  <|>
  do rest <- freshTerm
     (list #=) =<< cons __ (pure rest)
     iright left right rest

member x list =
  do join $ (list #=) <$> cons (pure x) __
  <|>
  do tail <- freshTerm
     (list #=) =<< cons __ (pure tail)
     member x tail

__ :: MonadVar m => m (Term m f)
__ = freshTerm

class Traversable f => Unifiable f where
  zipMatch :: f a -> f b -> Maybe (f (a, b))
  default zipMatch :: (Generic1 f, GUnifiable (Rep1 f)) => f a -> f b -> Maybe (f (a, b))
  zipMatch a b = to1 <$> gzipMatch (from1 a) (from1 b)

class GUnifiable f where
  gzipMatch :: f a -> f b -> Maybe (f (a, b))

instance GUnifiable V1 where
  gzipMatch _ _ = Nothing

instance GUnifiable U1 where
  gzipMatch U1 U1 = Just U1

instance GUnifiable Par1 where
  gzipMatch (Par1 a) (Par1 b) = Just $ Par1 (a, b)

instance Unifiable f => GUnifiable (Rec1 f) where
  gzipMatch (Rec1 a) (Rec1 b) = Rec1 <$> zipMatch a b

instance Eq c => GUnifiable (K1 i c) where
  gzipMatch (K1 a) (K1 b)
    | a == b = Just $ K1 a
    | otherwise = Nothing

instance GUnifiable a => GUnifiable (M1 i c a) where
  gzipMatch (M1 a) (M1 b) = M1 <$> gzipMatch a b

instance (GUnifiable a, GUnifiable b) => GUnifiable (a :+: b) where
  gzipMatch (L1 a) (L1 b) = L1 <$> gzipMatch a b
  gzipMatch (R1 a) (R1 b) = R1 <$> gzipMatch a b
  gzipMatch _ _ = Nothing

instance (GUnifiable a, GUnifiable b) => GUnifiable (a :*: b) where
  gzipMatch (a1 :*: a2) (b1 :*: b2) =
    (:*:) <$> gzipMatch a1 b1 <*> gzipMatch a2 b2

instance (Unifiable f, GUnifiable g) => GUnifiable (f :.: g) where
  gzipMatch (Comp1 a) (Comp1 b) =
    fmap Comp1 . traverse (uncurry gzipMatch) =<< zipMatch a b

class IsVar var where
  sameVar :: var a -> var a -> Bool
  default sameVar :: Eq (var a) => var a -> var a -> Bool
  sameVar = (==)

class (IsVar (Var m), Monad m) => MonadVar m where
  type Var m :: * -> *
  freshVar :: m (Var m a)
  readVar :: Var m a -> m (Maybe a)
  writeVar :: Var m a -> a -> m ()

data Term m f
  = Var !(Var m (Term m f))
  | Embed (f (Term m f))

type instance Base (Term m f) = f

instance Functor f => Unfoldable (Term m f) where
  embed = Embed

freshTerm :: MonadVar m => m (Term m f)
freshTerm = liftM Var freshVar

(#=) :: (Unifiable f, MonadPlus m, MonadVar m) => Term m f -> Term m f -> m ()
(#=) = unify (\ _ _ -> mzero)

unify :: (Unifiable f, MonadVar m) =>
         (f (Term m f) -> f (Term m f) -> m (Term m f)) ->
         Term m f -> Term m f -> m ()
unify f = \ t1 t2 -> do
  _ <- loop t1 t2
  return ()
  where
    loop t1 t2 = do
      x1 <- semiprune t1
      x2 <- semiprune t2
      case (x1, x2) of
        (_ :*! UnwrittenVar v1, t2' :*! UnwrittenVar v2)
          | sameVar v1 v2 ->
            return t2'
          | otherwise -> do
            writeVar v1 t2'
            return t2'
        (_ :*! UnwrittenVar v1, t2' :*! WrittenVar _ _) -> do
          writeVar v1 t2'
          return t2'
        (t1' :*! WrittenVar _ _, _ :*! UnwrittenVar v2) -> do
          writeVar v2 t1'
          return t1'
        (_ :*! WrittenVar v1 f1, t2' :*! WrittenVar v2 f2)
          | sameVar v1 v2 ->
            return t2'
          | otherwise -> do
            writeVar v2 =<< match f1 f2
            writeVar v1 t2'
            return t2'
        (t1' :*! UnwrittenVar v1, t2' :*! Base _) -> do
          writeVar v1 t2'
          return t1'
        (t1' :*! WrittenVar v1 f1, _ :*! Base f2) -> do
          writeVar v1 =<< match f1 f2
          return t1'
        (_ :*! Base f1, t2' :*! WrittenVar v2 f2) -> do
          writeVar v2 =<< match f1 f2
          return t2'
        (t1' :*! Base _, t2' :*! UnwrittenVar v2) -> do
          writeVar v2 t1'
          return t2'
        (_ :*! Base f1, _ :*! Base f2) ->
          match f1 f2
    match f1 f2 =
      maybe (f f1 f2) (liftM embed . mapM (uncurry loop)) $ zipMatch f1 f2

freeze :: (Traversable f, MonadVar m) => Term m f -> m (Fix f)
freeze = loop
  where
    loop = semiprune >=> \ x -> case x of
      _ :*! UnwrittenVar _ -> error "freeze: unwritten var"
      _ :*! WrittenVar _ f -> liftM Fix $ mapM loop f
      _ :*! Base f -> liftM Fix $ mapM loop f

unfreeze :: Functor f => Fix f -> Term m f
unfreeze = refix

semiprune :: MonadVar m => Term m f -> m (Pair (Term m f) (Semipruned m (Term m f)))
semiprune t0 = case t0 of
  Embed f -> return $ t0 :*! Base f
  Var v -> loop t0 v
  where
    loop t v = do
      a <- readVar v
      case a of
        Nothing ->
          return $! t :*! UnwrittenVar v
        Just t'@(Var v') -> do
          x@(t'' :*! _) <- loop t' v'
          writeVar v t''
          return x
        Just (Embed f) ->
          return $! t :*! (WrittenVar v f)

data Pair a b = !a :*! !b

data Semipruned m t
  = UnwrittenVar !(Var m t)
  | WrittenVar !(Var m t) (Base t t)
  | Base !(Base t t)
