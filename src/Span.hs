module Span where

import Data.Text (Text)
import Data.Text.Lazy (pack)

data Span = Span
  { start :: Int,
    end :: Int
  }
  deriving (Show, Eq)

data Spanned a = Spanned
  { value :: a,
    span :: Span
  }
  deriving (Show, Eq)

-- prettySpanned :: (Show a) => Spanned a -> Text
-- prettySpanned (Spanned val (Span s e)) = pack $ show val ++ " @ " ++ show s ++ ".." ++ show e

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s