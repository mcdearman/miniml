module MMC.Symbol (Symbol (..), Entry (..), SymbolTable (..)) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import MMC.Common (Unique)
import MMC.Ty (Ty)

data Symbol = Symbol {symId :: !Unique}
  deriving (Show, Eq, Ord)

data Entry = Entry
  { entryName :: !Text,
    entryScope :: !Int,
    entryType :: Ty
  }
  deriving (Show, Eq)

data SymbolTable
  = SymbolTable
  { symTableEntries :: HashMap Symbol Entry,
    symTableNextId :: !Unique
  }
  deriving (Show, Eq)