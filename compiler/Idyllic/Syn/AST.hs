module Idyllic.Syn.AST where

import Data.Text (Text)

data Expr
  = ExprInt Int
  | ExprVar Text
  | ExprLet Bind Expr
  | ExprLam Text Expr
  | ExprApp Expr Expr
  deriving (Show, Eq, Ord)

data Bind = BindName Text Expr | BindFun Text [Text] Expr deriving (Show, Eq, Ord)
