{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MMC.Miniparsec.Internal
  ( ParsecT (..),
  -- Result (..),
  -- runParsecT,
  -- Parsec,
  -- runParsec,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Class
import MMC.Miniparsec.State (State)
import MMC.Miniparsec.Stream (Stream)

newtype ParsecT e s d m a = ParsecT {runParsecT :: State s d e -> ([e], (m a, State s d e))}

instance (Monad m) => Functor (ParsecT e s d m) where
  fmap f (ParsecT p) = ParsecT $ \st -> case p st of
    (es, (a, st')) -> (es, (f <$> a, st'))
  {-# INLINE fmap #-}

instance (Monad m) => Applicative (ParsecT e s d m) where
  pure a = ParsecT $ \st -> ([], (pure a, st))
  {-# INLINE pure #-}

  ParsecT p <*> ParsecT p' = ParsecT $ \st -> case p st of
    (es, (f, st')) -> case p' st' of
      (es', (a, st'')) -> (es <> es', (f <*> a, st''))
  {-# INLINE (<*>) #-}

-- instance (Ord e, Stream s) => Alternative (ParsecT e s d m) where
--   empty = mzero
--   (<|>) = mplus

-- instance (Monad m) => MonadPlus (ParsecT e s d m) where
--   mzero = ParsecT $ \st -> ([], (pure Nothing, st))
--   {-# INLINE mzero #-}

-- instance (Monad m) => Monad (ParsecT e s d m) where
--   return = pure
--   {-# INLINE return #-}

--   ParsecT p >>= f = ParsecT $ \st -> case p st of
--     _ -> _
--   {-# INLINE (>>=) #-}