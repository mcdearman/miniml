module MMC.AST
  ( Prog,
    Module (..),
    LDecl,
    Decl (..),
    LDef,
    Def (..),
    LExpr,
    Expr (..),
    LUnaryOp,
    UnaryOp (..),
    unOpName,
    LBinaryOp,
    BinaryOp (..),
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
    moduleDecls :: [LDecl]
  }

type LDecl = Located Decl

data Decl
  = DeclSig !Ident LTypeAnno
  | DeclDef LDef

type LDef = Located Def

data Def
  = Def
      { defPat :: !LPattern,
        defBody :: LExpr,
        defWhereBinds :: [LDef]
      }
  | FunDef
      { funName :: !Ident,
        funParams :: [LPattern],
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
  | Unary !LUnaryOp LExpr
  | Binary !LBinaryOp LExpr LExpr
  | If LExpr LExpr LExpr
  | Match LExpr [(LPattern, LExpr)]
  | List [LExpr]
  | Tuple [LExpr]
  | Record (Maybe Ident) [(Ident, LExpr)]
  | RecordAccess LExpr Ident
  | RecordUpdate LExpr [(Ident, LExpr)]
  | Unit
  deriving (Show, Eq)

type LUnaryOp = Located UnaryOp

data UnaryOp
  = UnOpNeg
  deriving (Show, Eq)

unOpName :: UnaryOp -> Text
unOpName UnOpNeg = "neg"

type LBinaryOp = Located BinaryOp

data BinaryOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpMod
  | BinOpEq
  | BinOpNeq
  deriving (Show, Eq)

binOpName :: BinaryOp -> Text
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
