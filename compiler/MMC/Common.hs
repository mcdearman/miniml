module MMC.Common
  ( InputMode (..),
    Pretty (..),
    Result (..),
    Loc (..),
    defaultLoc,
    Located (..),
    unLoc,
    getLoc,
    Unique (..),
  )
where

import Data.Text (Text, pack)

data InputMode
  = InputModeFile Text
  | InputModeInteractive
  deriving (Show, Eq)

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

data Loc = Loc {locStart :: Int, locEnd :: Int}
  deriving (Show, Eq, Ord)

defaultLoc :: Loc
defaultLoc = Loc 0 0

instance Pretty Loc where
  pretty (Loc s e) = pack $ show s <> ".." <> show e

instance Semigroup Loc where
  Loc s1 e1 <> Loc s2 e2 = Loc (min s1 s2) (max e1 e2)

data Located a = Located a Loc
  deriving (Show, Eq, Ord)

unLoc :: Located a -> a
unLoc (Located v _) = v

getLoc :: Located a -> Loc
getLoc (Located _ loc) = loc

instance (Pretty a) => Pretty (Located a) where
  pretty (Located v s) = pretty v <> " @ " <> pretty s

instance Functor Located where
  fmap f (Located v s) = Located (f v) s

data Unique = Id Int deriving (Show, Eq, Ord)

locToLineCol :: [Text] -> Loc -> (Int, Int)
locToLineCol lines (Loc start end) = undefined
