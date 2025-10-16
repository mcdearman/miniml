{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Idyllic.Typing.Infer where

import Control.Monad.ST (ST, runST)
import Data.Map (Map)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Idyllic.Rename.Symbol (Symbol)
import Idyllic.Typing.Ty (ClosedTy (..), Level, Meta (..), Ty (..))

data TyError s
  = UnificationFail (Ty s) (Ty s)
  | InfiniteTy Int (Ty s)
  | UnboundVariable Symbol
  deriving (Eq)

data Scheme = Forall [Int] ClosedTy deriving (Show, Eq, Ord)

data InferState s = InferState {fresh :: !Int, level :: !Level}

type Env s = Map Symbol (Ty s)

newtype Infer s a = Infer {runInferM :: STRef s (InferState s) -> ST s a} deriving (Functor)

instance Applicative (Infer s) where
  pure x = Infer $ \_ -> pure x
  Infer f <*> Infer a = Infer $ \st -> f st <*> a st

instance Monad (Infer s) where
  Infer m >>= f = Infer $ \st -> do
    a <- m st
    runInferM (f a) st

runInfer :: (forall s. Infer s a) -> a
runInfer m = runST $ do
  st <- newSTRef (InferState 0 0)
  runInferM m st

freshMeta :: Infer s (Ty s)
freshMeta = Infer $ \st -> do
  s <- readSTRef st
  r <- newSTRef (Unbound (fresh s) (level s))
  writeSTRef st s {fresh = fresh s + 1}
  pure (TyMeta r)

withLevel :: Infer s a -> Infer s a
withLevel (Infer m) = Infer $ \st -> do
  s <- readSTRef st
  writeSTRef st s {level = level s + 1}
  a <- m st
  modifySTRef' st (\s' -> s' {level = level s' - 1})
  pure a

zonk :: Ty s -> Infer s (Ty s)
zonk (TyCon c) = pure (TyCon c)
zonk (TyApp f x) = TyApp <$> zonk f <*> zonk x
zonk t@(TyMeta r) = Infer $ \_ -> do
  cell <- readSTRef r
  case cell of
    Bound ty -> do
      -- path compression
      -- (optional: zonk ty first to keep the path short)
      pure ty
    Unbound {} -> pure t