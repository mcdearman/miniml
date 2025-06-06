module MMC.AST
  ( Prog,
    Module (..),
    LDecl,
    Decl (..),
    LDef,
    Def (..),
    Bind (..),
    LExpr,
    Expr (..),
    LUnaryOp,
    UnaryOp (..),
    unaryOpName,
    LBinaryOp,
    BinaryOp (..),
    binaryOpName,
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
  = DeclImport !Ident
  | DeclExport !Ident
  | DeclClassDecl !ClassDecl
  deriving (Show, Eq)

data ClassDef
  = ClassDef
  { className :: Ident,
    classSuperclasses :: [Ident],
    classTypeParams :: [Ident],
    classMethods :: [LClassDecl]
  }
  deriving (Show, Eq)

type LClassDecl = Located ClassDecl

data ClassDecl
  = ClassDeclSig ![Ident] LTypeAnno
  | ClassDeclDef LDef
  deriving (Show, Eq)

type LDef = Located Def

data Def
  = Def
  { defBind :: Bind,
    defBody :: Rhs,
    defWhereBinds :: [LClassDecl]
  }
  deriving (Show, Eq)

type LExpr = Located Expr

data Rhs = Expr LExpr | Guarded [LGuard]
  deriving (Show, Eq)

data Expr
  = Lit !Lit
  | Var !Ident
  | App LExpr LExpr
  | Lam [LPattern] LExpr
  | Let Bind LExpr LExpr
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

data Bind
  = BindPattern !LPattern
  | BindFun !Ident [LPattern]
  deriving (Show, Eq)

data MatchGroup = MatchGroup
  { matchGroupScrutinee :: LExpr,
    matchGroupAlts :: [(LPattern, LExpr)]
  }
  deriving (Show, Eq)

type LGuard = Located Guard

data Guard = Guard
  { guardPattern :: LPattern,
    guardExpr :: LExpr
  }
  deriving (Show, Eq)

type LUnaryOp = Located UnaryOp

data UnaryOp
  = UnaryOpNeg
  deriving (Show, Eq)

unaryOpName :: UnaryOp -> Text
unaryOpName UnaryOpNeg = "neg"

type LBinaryOp = Located BinaryOp

data BinaryOp
  = BinaryOpAdd
  | BinaryOpSub
  | BinaryOpMul
  | BinaryOpDiv
  | BinaryOpMod
  | BinaryOpEq
  | BinaryOpNeq
  deriving (Show, Eq)

binaryOpName :: BinaryOp -> Text
binaryOpName BinaryOpAdd = "add"
binaryOpName BinaryOpSub = "sub"
binaryOpName BinaryOpMul = "mul"
binaryOpName BinaryOpDiv = "div"
binaryOpName BinaryOpMod = "mod"
binaryOpName BinaryOpEq = "eq"
binaryOpName BinaryOpNeq = "neq"

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
