module Idyllic.Rename.HIR where

import Idyllic.Utils.Symbol (Symbol)

data Expr
  = ExprInt Int
  | ExprVar Symbol
  | ExprLet [Bind] Expr
  | ExprLam Symbol Expr
  | ExprApp Expr Expr
  | ExprError
  deriving (Show, Eq, Ord)

data Bind = BindName Symbol Expr | BindFun Symbol [Symbol] Expr deriving (Show, Eq, Ord)