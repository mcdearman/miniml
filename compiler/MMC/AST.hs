module MMC.AST
  ( Prog,
    Module (..),
    LDef,
    Def (..),
    LExpr,
    Expr (..),
    UnaryOp,
    UnOpSort (..),
    unOpName,
    BinaryOp,
    BinOpSort (..),
    binOpName,
    LTypeAnno,
    TypeAnno (..),
    LPattern,
    Pattern (..),
    Ident (..),
    Lit (..),
  )
where

import Data.Text (Text)
import MMC.Common (Located)

type Prog = Located Module

data Module = Module
  { moduleName :: !Ident,
    moduleDefs :: [LDef]
  }

type LDef = Located Def

data Def
  = Def
      { defPat :: !LPattern,
        defBody :: LExpr,
        defWhereBinds :: [LDef]
      }
  | FunDef
      { funName :: !Ident,
        funTypeAnno :: Maybe LTypeAnno,
        funArgs :: [LPattern],
        funBody :: LExpr,
        funWhereBinds :: [LDef]
      }
  deriving (Show, Eq)

type LExpr = Located Expr

data Expr
  = Lit !Lit
  | Var !Ident
  | App LExpr LExpr
  | Lam [LPattern] LExpr
  | Let LPattern LExpr LExpr
  | Fun !Ident [LPattern] LExpr LExpr
  | Unary !UnaryOp LExpr
  | Binary !BinaryOp LExpr LExpr
  | If LExpr LExpr LExpr
  | Match LExpr [(LPattern, LExpr)]
  | List [LExpr]
  | Tuple [LExpr]
  | Record (Maybe Ident) [(Ident, LExpr)]
  | RecordAccess LExpr Ident
  | RecordUpdate LExpr [(Ident, LExpr)]
  | Unit
  deriving (Show, Eq)

type UnaryOp = Located UnOpSort

data UnOpSort
  = UnOpNeg
  deriving (Show, Eq)

unOpName :: UnOpSort -> Text
unOpName UnOpNeg = "neg"

type BinaryOp = Located BinOpSort

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

type LTypeAnno = Located TypeAnno

data TypeAnno
  = TypeAnnoVar !Ident
  | TypeAnnoIdent !Ident
  | TypeAnnoFun !LTypeAnno !LTypeAnno
  | TypeAnnoList !LTypeAnno
  | TypeAnnoTuple [LTypeAnno]
  | TypeAnnoUnit
  deriving (Show, Eq)

type LPattern = Located Pattern

data Pattern
  = PatternWildcard
  | PatternLit !Lit
  | PatternIdent !Ident
  | PatternCons !Ident [LPattern]
  | PatternAs !Ident LPattern
  | PatternList [LPattern]
  | PatternTuple [LPattern]
  | PatternUnit
  deriving (Show, Eq)

newtype Ident = Ident (Located Text) deriving (Show, Eq, Ord)

data Lit
  = Int Integer
  | Bool !Bool
  | String !Text
  deriving (Show, Eq)
