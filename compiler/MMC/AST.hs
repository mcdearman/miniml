module MMC.AST
  ( Prog,
    Module (..),
    Def,
    DefSort (..),
    FunDef,
    FunDefSort (..),
    Expr,
    ExprSort (..),
    UnaryOp,
    UnOpSort (..),
    unOpName,
    BinaryOp,
    BinOpSort (..),
    binOpName,
    TypeAnno,
    TypeAnnoSort (..),
    Pattern,
    PatternSort (..),
    Ident (..),
    Lit (..),
  )
where

import Data.Text (Text)
import MMC.Common (Spanned)

type Prog = Spanned Module

data Module = Module
  { moduleName :: !Ident,
    moduleDefs :: [Def],
    moduleFunDefs :: [FunDef]
  }

type Def = Spanned DefSort

data DefSort = Def {defPat :: !Pattern, defBody :: Expr} deriving (Show, Eq)

type FunDef = Spanned FunDefSort

data FunDefSort = FnDef
  { fnName :: !Ident,
    fnTypeAnno :: Maybe TypeAnno,
    fnArgs :: [Pattern],
    fnBody :: Expr
  }
  deriving (Show, Eq)

type Expr = Spanned ExprSort

data ExprSort
  = Lit !Lit
  | Var !Ident
  | App Expr Expr
  | Lam [Pattern] Expr
  | Let Pattern Expr Expr
  | Fun !Ident [Pattern] Expr Expr
  | Unary !UnaryOp Expr
  | Binary !BinaryOp Expr Expr
  | If Expr Expr Expr
  | Match Expr [(Pattern, Expr)]
  | List [Expr]
  | Tuple [Expr]
  | Record (Maybe Ident) [(Ident, Expr)]
  | Cast !TypeAnno Expr
  | Unit
  deriving (Show, Eq)

type UnaryOp = Spanned UnOpSort

data UnOpSort
  = UnOpNeg
  deriving (Show, Eq)

unOpName :: UnOpSort -> Text
unOpName UnOpNeg = "neg"

type BinaryOp = Spanned BinOpSort

data BinOpSort
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpMod
  | BinOpEq
  | BinOpNeq
  deriving (Show, Eq)

binOpName :: BinOpSort -> Text
binOpName BinOpAdd = "add"
binOpName BinOpSub = "sub"
binOpName BinOpMul = "mul"
binOpName BinOpDiv = "div"
binOpName BinOpMod = "mod"
binOpName BinOpEq = "eq"
binOpName BinOpNeq = "neq"

type TypeAnno = Spanned TypeAnnoSort

data TypeAnnoSort
  = TypeAnnoVar !Ident
  | TypeAnnoIdent !Ident
  | TypeAnnoFun !TypeAnno !TypeAnno
  | TypeAnnoList !TypeAnno
  | TypeAnnoTuple [TypeAnno]
  | TypeAnnoUnit
  deriving (Show, Eq)

type Pattern = Spanned PatternSort

data PatternSort
  = PatternWildcard
  | PatternLit !Lit
  | PatternIdent !Ident
  | PatternCons !Ident [Pattern]
  | PatternAs !Ident Pattern
  | PatternList [Pattern]
  | PatternTuple [Pattern]
  | PatternUnit
  deriving (Show, Eq)

newtype Ident = Ident (Spanned Text) deriving (Show, Eq, Ord)

data Lit
  = Int Integer
  | Bool !Bool
  | String !Text
  deriving (Show, Eq)