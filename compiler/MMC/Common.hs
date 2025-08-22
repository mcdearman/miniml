{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
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
    LineIndex (..),
    buildLineIndex,
    offsetToLineCol,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

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

data LineIndex
  = LineIndex {lineStarts :: !(U.Vector Int)}
  deriving (Show, Eq)

buildLineIndex :: Text -> LineIndex
buildLineIndex txt = LineIndex (U.fromList (0 : scan txt))
  where
    scan t = [i + 1 | (i, c) <- zip [0 ..] (T.unpack t), c == '\n']

offsetToLineCol :: LineIndex -> Int -> (Int, Int)
offsetToLineCol (LineIndex starts) !offset =
  let !i = binarySearch starts offset
      !lineStart = starts U.! i
      !col = offset - lineStart + 1
   in (i + 1, col)

{-# INLINE binarySearch #-}
binarySearch :: U.Vector Int -> Int -> Int
binarySearch !v !x = go 0 (U.length v - 1)
  where
    go !low !high
      | low > high = high
      | midVal == x = mid
      | midVal < x = go (mid + 1) high
      | otherwise = go low (mid - 1)
      where
        mid = (low + high) `div` 2
        midVal = v U.! mid
