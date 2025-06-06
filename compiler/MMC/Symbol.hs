module MMC.Symbol where

import Data.Text (Text)

data Symbol
  = Symbol
  { symName :: !Text,
    symType :: !Int,
    symKind :: !Int
  }
  deriving (Show, Eq, Ord)

data SymbolTable
  = SymbolTable
  { symTable :: [(Text, Symbol)],
    symNextId :: !Int
  }
  deriving (Show, Eq)