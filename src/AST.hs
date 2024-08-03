module AST where

data Def = Def String Expr deriving (Show)

data Expr
  = ELit Lit
  | EVar String
  | EApp Expr Expr
  | ELam String Expr
  | ELet String Expr Expr
  deriving (Show)

data Lit
  = LInt Integer
  | LBool Bool
  | LString String
  deriving (Show)
