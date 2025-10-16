module Idyllic.Rename.HIR where

import Idyllic.Rename.Symbol (Symbol)
import Idyllic.Utils.Span (Span)

type NodeId = Int

type Expr = HirNode ExprKind

data HirNode a = HirNode
  { nodeId :: !NodeId,
    nodeKind :: a,
    nodeSpan :: !Span
  }
  deriving (Show, Eq, Ord)

data ExprKind
  = ExprInt Int
  | ExprVar Symbol
  | ExprLet [Bind] Expr
  | ExprLam Symbol Expr
  | ExprApp Expr Expr
  | ExprError
  deriving (Show, Eq, Ord)

data Bind = BindName Symbol Expr | BindFun Symbol [Symbol] Expr deriving (Show, Eq, Ord)