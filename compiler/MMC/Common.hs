module MMC.Common (Pretty, Result (..), Span (..), defaultSpan, Spanned (..), Unique (..)) where

import Data.Text (Text, pack)
import Data.Word (Word32)

class Pretty a where
  pretty :: a -> Text

instance (Pretty a) => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x : xs) = pretty x <> ",\n" <> pretty xs

data Result e v
  = Err e
  | Ok v
  deriving (Eq, Show)

instance Functor (Result e) where
  fmap _ (Err e) = Err e
  fmap f (Ok v) = Ok (f v)

instance Applicative (Result e) where
  pure = Ok
  Err e <*> _ = Err e
  _ <*> Err e = Err e
  Ok f <*> Ok v = Ok (f v)

instance Monad (Result e) where
  Err e >>= _ = Err e
  Ok v >>= f = f v

data Span = Span {start :: Int, end :: Int}
  deriving (Show, Eq, Ord)

defaultSpan :: Span
defaultSpan = Span 0 0

instance Pretty Span where
  pretty (Span s e) = pack $ show s <> ".." <> show e

instance Semigroup Span where
  Span s1 e1 <> Span s2 e2 = Span (min s1 s2) (max e1 e2)

data Spanned a = Spanned
  { spannedVal :: a,
    span :: Span
  }
  deriving (Show, Eq, Ord)

instance (Pretty a) => Pretty (Spanned a) where
  pretty (Spanned v s) = pretty v <> " @ " <> pretty s

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s

newtype Unique = Id Int deriving (Show, Eq, Ord)