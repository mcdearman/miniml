module MMC.Utils.Symbol (Symbol (..), Entry (..), SymbolTable (..)) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
-- import MMC.Typing.Ty (Ty)
import MMC.Utils.Unique (Unique)

data Symbol = Symbol {symId :: !Unique}
  deriving (Show, Eq, Ord)

data Entry = Entry
  { entryName :: !Text,
    entrySort :: !EntrySort,
    entryScope :: !Int
    -- entryType :: Ty
  }
  deriving (Show, Eq)

data EntrySort
  = EntrySortType
  | EntrySortValue
  | EntrySortData
  | EntrySortConstructor
  | EntrySortFunction
  | EntrySortModule
  | EntrySortClass
  | EntrySortInstance
  deriving (Show, Eq, Ord)

data SymbolTable
  = SymbolTable
  { symTableEntries :: HashMap Symbol Entry,
    symTableNextId :: !Unique
  }
  deriving (Show, Eq)