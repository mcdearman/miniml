{-# LANGUAGE GADTs #-}

module AST where

data Def a = Def String (Expr a)

-- data Expr
--   = ELit Lit
--   | EVar String
--   | EApp Expr Expr
--   | ELam String Expr
--   | ELet String Expr Expr
--   deriving (Show)

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: (Eq a) => Expr a -> Expr a -> Expr Bool

data Lit
  = LInt Integer
  | LBool Bool
  | LString String
  deriving (Show)
