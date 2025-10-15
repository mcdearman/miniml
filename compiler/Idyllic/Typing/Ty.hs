module Idyllic.Typing.Ty where

import Data.STRef (STRef)
import Data.Text (Text)

type MetaId = Int

type Level = Int

data Meta s = Bound (Ty s) | Unbound !MetaId !Level deriving (Eq)

data Ty s
  = TyMeta (STRef s (Meta s))
  | TyCon Text
  | TyApp (Ty s) (Ty s)
  deriving (Eq)

pattern TyArr :: Ty s -> Ty s -> Ty s
pattern TyArr a b = TyApp (TyApp (TyCon "->") a) b

data ClosedTy
  = ClosedVar Int
  | ClosedCon Text
  | ClosedApp ClosedTy ClosedTy
  deriving (Show, Eq, Ord)
