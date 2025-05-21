{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MMC.Parsec.Internal
  ( ParsecT (..),
  -- Result (..),
  -- runParsecT,
  -- Parsec,
  -- runParsec,
  )
where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Class
import MMC.Parsec.State (State)

-- | Parse result: recoverable errors or hard failure for backtracking
-- data Result e a
--   = Fail
--   | Result [e] a
--   deriving (Show, Functor)

-- instance (Semigroup a) => Semigroup (Result e a) where
--   Fail <> r = r
--   l <> Fail = l
--   Result es1 v1 <> Result es2 v2 = Result (es1 <> es2) (v1 <> v2)

-- instance (Monoid a) => Monoid (Result e a) where
--   mempty = Result [] mempty

newtype ParsecT e s d m a = ParsecT {runParsecT :: State s d e -> ([e], (m a, State s d e))}

instance (Monad m) => Functor (ParsecT e s d m) where
  fmap f (ParsecT p) = ParsecT $ \st -> case p st of
    (es, (a, st')) -> (es, (f <$> a, st'))
  {-# INLINE fmap #-}

instance (Monad m) => Applicative (ParsecT e s d m) where
  pure a = ParsecT $ \st -> ([], (pure a, st))
  {-# INLINE pure #-}

  ParsecT p1 <*> ParsecT p2 = ParsecT $ \st -> case p1 st of
    (es1, (f, st')) -> case p2 st' of
      (es2, (a, st'')) -> (es1 <> es2, (f <*> a, st''))
  {-# INLINE (<*>) #-}
