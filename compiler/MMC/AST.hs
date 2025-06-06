module MMC.AST
  ( Prog,
    Module (..),
    LDecl,
    Decl (..),
    LRecordDef,
    RecordDef (..),
    LClassDef,
    ClassDef (..),
    LClassDecl,
    ClassDecl (..),
    LSig,
    Sig (..),
    Rhs (..),
    LExpr,
    Expr (..),
    LBind,
    Bind (..),
    Alt (..),
    LGuard,
    Guard (..),
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
  { moduleName :: !Text,
    moduleDecls :: [LDecl]
  }

type LDecl = Located Decl

data Decl
  = DeclImport ![Ident]
  | DeclExport !Ident
  | DeclClassDecl LClassDecl
  | DeclClassDef LClassDef
  | DeclRecordDef LRecordDef
  deriving (Show, Eq)

type LRecordDef = Located RecordDef

data RecordDef = RecordDef
  { recordName :: !Ident,
    recordTypeParams :: ![Ident],
    recordFields :: [(Ident, LTypeAnno)]
  }
  deriving (Show, Eq)

type LClassDef = Located ClassDef

data ClassDef
  = ClassDef
  { className :: !Ident,
    classSuperclasses :: ![Ident],
    classTypeParams :: ![Ident],
    classDecls :: [LClassDecl]
  }
  deriving (Show, Eq)

type LClassDecl = Located ClassDecl

data ClassDecl
  = ClassDeclSig LSig
  | ClassDeclBind LBind
  deriving (Show, Eq)

type LSig = Located Sig

data Sig = Sig
  { sigNames :: ![Ident],
    sigTypeParams :: ![Ident],
    sigTypeAnno :: LTypeAnno
  }
  deriving (Show, Eq)

data Rhs = RhsExpr LExpr | RhsGuard [LGuard]
  deriving (Show, Eq)

type LExpr = Located Expr

data Expr
  = Lit !Lit
  | Var !Ident
  | App LExpr LExpr
  | Lam [LPattern] LExpr
  | Let [LBind] LExpr
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

type LBind = Located Bind

data Bind
  = BindPattern !LPattern Rhs [LClassDecl]
  | BindFun !Ident [Alt] [LClassDecl]
  deriving (Show, Eq)

data Alt = Alt
  { altPatterns :: [LPattern],
    altExpr :: Rhs
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
