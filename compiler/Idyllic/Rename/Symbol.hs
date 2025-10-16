module Idyllic.Rename.Symbol where

import Data.Map (Map)
import Data.Text (Text)
import Idyllic.Utils.Span (Span)

data Symbol = Symbol
  { symbolId :: Int,
    symbolSpan :: Span
  }
  deriving (Show, Eq, Ord)

data SymbolTable = SymbolTable
  { nextId :: Int,
    symbols :: Map Text Symbol
  }
  deriving (Show, Eq, Ord)

emptyTable :: SymbolTable
emptyTable = SymbolTable 0 mempty
