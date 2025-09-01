module MMC.Syn.AST
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
    LInstanceDecl,
    InstanceDecl (..),
    LSig,
    Sig (..),
    Rhs (..),
    LExpr,
    Expr (..),
    LBind,
    Bind (..),
    LAlt,
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
import MMC.Utils.Span (Spanned)

type Prog = Spanned Module

data Module = Module
  { moduleName :: !Text,
    moduleDecls :: [LDecl]
  }
  deriving (Show, Eq)

type LDecl = Spanned Decl

data Decl
  = DeclImport ![Ident]
  | DeclExport !Ident
  | DeclClassDecl LClassDecl
  | DeclClassDef LClassDef
  | DeclInstanceDecl LInstanceDecl
  | DeclRecordDef LRecordDef
  deriving (Show, Eq)

type LRecordDef = Spanned RecordDef

data RecordDef = RecordDef
  { recordName :: !Ident,
    recordTypeParams :: ![Ident],
    recordFields :: [(Ident, LTypeAnno)]
  }
  deriving (Show, Eq)

type LClassDef = Spanned ClassDef

data ClassDef
  = ClassDef
  { className :: !Ident,
    classSuperclasses :: ![Ident],
    classTypeParams :: ![Ident],
    classDecls :: [LClassDecl]
  }
  deriving (Show, Eq)

type LClassDecl = Spanned ClassDecl

data ClassDecl
  = ClassDeclSig LSig
  | ClassDeclBind LBind
  deriving (Show, Eq)

type LInstanceDecl = Spanned InstanceDecl

data InstanceDecl = InstanceDecl
  { instanceClass :: !Ident,
    instanceTypeParams :: ![Ident],
    instanceTypeAnno :: LTypeAnno,
    instanceDecls :: [LClassDecl]
  }
  deriving (Show, Eq)

type LSig = Spanned Sig

data Sig = Sig
  { sigNames :: ![Ident],
    sigTypeParams :: ![Ident],
    sigTypeAnno :: LTypeAnno
  }
  deriving (Show, Eq)

data Rhs = RhsExpr LExpr | RhsGuard [LGuard]
  deriving (Show, Eq)

type LExpr = Spanned Expr

data Expr
  = Lit !Lit
  | Var !Ident
  | App LExpr LExpr
  | Lam [LPattern] LExpr
  | Let [LBind] LExpr
  | Do [LExpr]
  | Unary !LUnaryOp LExpr
  | Binary !LBinaryOp LExpr LExpr
  | If LExpr LExpr LExpr
  | Match LExpr [(LPattern, LExpr)]
  | List [LExpr]
  | Tuple [LExpr]
  | Cons Ident [LExpr]
  | Record (Maybe Ident) [(Ident, LExpr)]
  | RecordAccess LExpr Ident
  | RecordUpdate LExpr [(Ident, LExpr)]
  | Unit
  deriving (Show, Eq)

type LBind = Spanned Bind

data Bind
  = BindPattern !LPattern Rhs [LClassDecl]
  | BindFun !Ident [LAlt] [LClassDecl]
  deriving (Show, Eq)

type LAlt = Spanned Alt

data Alt = Alt
  { altPatterns :: [LPattern],
    altExpr :: Rhs
  }
  deriving (Show, Eq)

type LGuard = Spanned Guard

data Guard = Guard
  { guardPattern :: LPattern,
    guardExpr :: LExpr
  }
  deriving (Show, Eq)

type LUnaryOp = Spanned UnaryOp

data UnaryOp
  = UnaryOpNeg
  deriving (Show, Eq)

unaryOpName :: UnaryOp -> Text
unaryOpName UnaryOpNeg = "neg"

type LBinaryOp = Spanned BinaryOp

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
binaryOpName BinaryOpAdd = "+"
binaryOpName BinaryOpSub = "-"
binaryOpName BinaryOpMul = "*"
binaryOpName BinaryOpDiv = "/"
binaryOpName BinaryOpMod = "%"
binaryOpName BinaryOpEq = "=="
binaryOpName BinaryOpNeq = "!="

type LTypeAnno = Spanned TypeAnno

data TypeAnno
  = TypeAnnoVar !Ident
  | TypeAnnoIdent !Ident
  | TypeAnnoFun !LTypeAnno !LTypeAnno
  | TypeAnnoList !LTypeAnno
  | TypeAnnoTuple [LTypeAnno]
  | TypeAnnoUnit
  deriving (Show, Eq)

type LPattern = Spanned Pattern

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

newtype Ident = Ident (Spanned Text) deriving (Show, Eq, Ord)

data Lit
  = Int Integer
  | Bool !Bool
  | String !Text
  deriving (Show, Eq)
