{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MMC.Parsec.Internal
  ( ParsecT (..),
    Result (..),
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
data Result e a
  = Fail
  | Result [e] a
  deriving (Show, Functor)

instance (Semigroup a) => Semigroup (Result e a) where
  Fail <> r = r
  l <> Fail = l
  Result es1 v1 <> Result es2 v2 = Result (es1 <> es2) (v1 <> v2)

instance (Monoid a) => Monoid (Result e a) where
  mempty = Result [] mempty

-- | Parser transformer: token stream `s`, errors `e`, in monad `m`
newtype ParsecT e s d m a = ParsecT
  { runParsecT ::
      forall b.
      State s d e ->
      (a -> State s d e -> m b) -> -- consumed OK
      (e -> State s d e -> m b) -> -- consumed error
      (a -> State s d e -> m b) -> -- empty OK
      (e -> State s d e -> m b) -> -- empty error
      m b
  }
  deriving (Functor)

-- -- | Applicative: sequential composition, short-circuit on Fail
-- instance (Monad m) => Applicative (ParsecT t e m) where
--   pure x = ParsecT $ \xs -> pure (Result [] x, xs)
--   ParsecT pf <*> ParsecT pa = ParsecT $ \xs -> do
--     (r1, xs1) <- pf xs
--     case r1 of
--       Fail -> pure (Fail, xs1)
--       Result es f -> do
--         (r2, xs2) <- pa xs1
--         case r2 of
--           Fail -> pure (Fail, xs2)
--           Result es' v -> pure (Result (es <> es') (f v), xs2)

-- -- | Monad: thread input, accumulate errors, short-circuit on Fail
-- instance (Monad m) => Monad (ParsecT t e m) where
--   return = pure
--   ParsecT p >>= k = ParsecT $ \xs -> do
--     (r1, xs1) <- p xs
--     case r1 of
--       Fail -> pure (Fail, xs1)
--       Result es v -> do
--         (r2, xs2) <- runParsecT (k v) xs1
--         case r2 of
--           Fail -> pure (Fail, xs2)
--           Result es' v' -> pure (Result (es <> es') v', xs2)

-- -- | Alternative: try first parser, if it fails then second
-- instance (Monad m) => Alternative (ParsecT t e m) where
--   empty = ParsecT $ \xs -> pure (Fail, xs)
--   p1 <|> p2 = ParsecT $ \xs -> do
--     (r1, xs1) <- runParsecT p1 xs
--     case r1 of
--       Fail -> runParsecT p2 xs
--       _ -> pure (r1, xs1)

-- -- | Lift an action from the underlying monad
-- instance MonadTrans (ParsecT t e) where
--   lift m = ParsecT $ \xs -> do
--     v <- m
--     pure (Result [] v, xs)
