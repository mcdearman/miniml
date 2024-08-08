module AST where

import Span

data Def = Def (Spanned String) ()

-- data Expr a
--   = ELit Lit
--   | EVar (a String)
--   | EApp (a (Expr a)) (a (Expr a))
--   | ELam (a String) (a (Expr a))
--   | ELet (a String) (a (Expr a)) (a (Expr a))
--   deriving (Show)
data Expr a where
  ELit :: Lit -> Expr a
  EVar :: Show a => a String -> Expr a
  EApp :: Show a => a (Expr a) -> a (Expr a) -> Expr a
  ELam :: Show a => a String -> a (Expr a) -> Expr a
  ELet :: a String -> a (Expr a) -> a (Expr a) -> Expr a
  deriving (Show)

data Lit
  = LInt Integer
  | LBool Bool
  | LString String
  deriving (Show)
