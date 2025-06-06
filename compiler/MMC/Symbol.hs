module MMC.Symbol (Symbol (..), Entry (..), SymbolTable (..)) where

import Data.Text (Text)
import MMC.Common (Unique)
import MMC.Ty (Ty)
import GHC.Exts (Int#)

data Symbol = Symbol {symId :: !Unique}
  deriving (Show, Eq, Ord)

data Entry = Entry
  { entryName :: !Text,
    entryScope :: Int#,
    entryType :: Ty
  }
  deriving (Show, Eq)

data SymbolTable
  = SymbolTable
  { symTableEntries :: [(Unique, Entry)],
    symTableNextId :: !Unique
  }
  deriving (Show, Eq)