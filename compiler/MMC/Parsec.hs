{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MMC.Parsec where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Data.Kind (Type)

class Stream s where
  data Token s :: Type

  take1' :: s -> Maybe (Token s, s)

  takeWhile' :: (Token s -> Bool) -> s -> (s, s)
  takeWhile' p xs = go xs
    where
      go xs' =
        case take1' xs' of
          Just (x, xs'') | p x -> let (ys, zs) = go xs'' in (x : ys, zs)
          _ -> ([], xs')



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

-- | Parser transformer: token stream `t`, errors `e`, in monad `m`
newtype ParsecT t e m a = ParsecT
  {runParsecT :: [t] -> m (Result e a, [t])}
  deriving (Functor)

-- | Pure parser (no effects)
type Parsec t e = ParsecT t e Identity

-- | Run a pure parser
runParsec :: Parsec t e a -> [t] -> (Result e a, [t])
runParsec p xs = runIdentity (runParsecT p xs)

-- | Applicative: sequential composition, short-circuit on Fail
instance (Monad m) => Applicative (ParsecT t e m) where
  pure x = ParsecT $ \xs -> pure (Result [] x, xs)
  ParsecT pf <*> ParsecT pa = ParsecT $ \xs -> do
    (r1, xs1) <- pf xs
    case r1 of
      Fail -> pure (Fail, xs1)
      Result es f -> do
        (r2, xs2) <- pa xs1
        case r2 of
          Fail -> pure (Fail, xs2)
          Result es' v -> pure (Result (es <> es') (f v), xs2)

-- | Monad: thread input, accumulate errors, short-circuit on Fail
instance (Monad m) => Monad (ParsecT t e m) where
  return = pure
  ParsecT p >>= k = ParsecT $ \xs -> do
    (r1, xs1) <- p xs
    case r1 of
      Fail -> pure (Fail, xs1)
      Result es v -> do
        (r2, xs2) <- runParsecT (k v) xs1
        case r2 of
          Fail -> pure (Fail, xs2)
          Result es' v' -> pure (Result (es <> es') v', xs2)

-- | Alternative: try first parser, if it fails then second
instance (Monad m) => Alternative (ParsecT t e m) where
  empty = ParsecT $ \xs -> pure (Fail, xs)
  p1 <|> p2 = ParsecT $ \xs -> do
    (r1, xs1) <- runParsecT p1 xs
    case r1 of
      Fail -> runParsecT p2 xs
      _ -> pure (r1, xs1)

-- | Lift an action from the underlying monad
instance MonadTrans (ParsecT t e) where
  lift m = ParsecT $ \xs -> do
    v <- m
    pure (Result [] v, xs)

-- | Get current offset
getOffset :: (Monad m) => ParsecT t e m Int
getOffset = ParsecT $ \xs -> do
  let offset = length xs
  pure (Result [] offset, xs)

-- | `single` t matches the single token t.
single ::
  (Monad m) =>
  -- | extract on success
  (t -> a) ->
  -- | error on failure
  (t -> e) ->
  -- | hole on failure
  a ->
  ParsecT t e m a
single extr errF hole = ParsecT $ \case
  (x : xs') ->
    let v = extr x
     in pure (Result [] v, xs')
  [] ->
    let e = errF (error "no token")
     in pure (Result [e] hole, [])

-- | A generic `any` combinator that records errors but does not Fail
any ::
  (Monad m) =>
  -- | extract on success
  (t -> a) ->
  -- | error on failure
  (t -> e) ->
  -- | hole on failure
  a ->
  ParsecT t e m a
any extr errF hole = ParsecT $ \case
  (x : xs') ->
    let v = extr x
     in pure (Result [] v, xs')
  [] ->
    let e = errF (error "no token")
     in pure (Result [e] hole, [])

-- | A generic `satisfy` combinator that records errors but does not Fail
satisfy ::
  (Monad m) =>
  -- | predicate
  (t -> Bool) ->
  -- | extract on success
  (t -> a) ->
  -- | error on failure
  (t -> e) ->
  -- | hole on failure
  a ->
  ParsecT t e m a
satisfy test extr errF hole = ParsecT $ \case
  (x : xs')
    | test x ->
        let v = extr x
         in pure (Result [] v, xs')
  (x : xs') ->
    let e = errF x
     in pure (Result [e] hole, xs')
  [] ->
    let e = errF (error "no token")
     in pure (Result [e] hole, [])

oneOf ::
  (Monad m, Eq t) =>
  -- | list of tokens
  [t] ->
  -- | extract on success
  (t -> a) ->
  -- | error on failure
  (t -> e) ->
  -- | hole on failure
  a ->
  ParsecT t e m a
oneOf ts extr errF hole = ParsecT $ \case
  (x : xs')
    | x `elem` ts ->
        let v = extr x
         in pure (Result [] v, xs')
  (x : xs') ->
    let e = errF x
     in pure (Result [e] hole, xs')
  [] ->
    let e = errF (error "no token")
     in pure (Result [e] hole, [])

between ::
  (Monad m, Eq t) =>
  -- | opening token
  t ->
  -- | closing token
  t ->
  -- | extract on success
  (t -> a) ->
  -- | error on failure
  (t -> e) ->
  -- | hole on failure
  a ->
  ParsecT t e m a
between open close extr errF hole = ParsecT $ \case
  (x : xs)
    | x == open ->
        let v = extr x
         in pure (Result [] v, xs)
  (x : xs) ->
    let e = errF x
     in pure (Result [e] hole, xs)
  [] ->
    let e = errF (error "no token")
     in pure (Result [e] hole, [])