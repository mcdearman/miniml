module Idyllic.Syn.AST where

import Data.Text (Text)
import Idyllic.Utils.Span (Span)

data SynNode a = SynNode
  { synNodeKind :: a,
    synNodeSpan :: !Span
  }
  deriving (Show, Eq, Ord)

type Expr = SynNode ExprKind

data ExprKind
  = ExprInt Int
  | ExprVar Ident
  | ExprLet Bind Expr
  | ExprLam Ident Expr
  | ExprApp Expr Expr
  deriving (Show, Eq, Ord)

data Bind = BindName Ident Expr | BindFun Ident [Ident] Expr deriving (Show, Eq, Ord)

type Ident = SynNode Text
