module MMC.Syn.AST
  ( Prog,
    Module (..),
    SDecl,
    Decl (..),
    SRecordDef,
    RecordDef (..),
    SClassDef,
    ClassDef (..),
    SClassDecl,
    ClassDecl (..),
    SInstancDecl,
    InstanceDecl (..),
    SSig,
    Sig (..),
    Rhs (..),
    SExpr,
    Expr (..),
    SBind,
    Bind (..),
    SAlt,
    Alt (..),
    SGuard,
    Guard (..),
    SAnno,
    Anno (..),
    SPat,
    Pat (..),
    Ident (..),
    Lit (..),
  )
where

import Data.Text (Text)
import MMC.Utils.Span (Spanned)

type Prog = Spanned Module

data Module = Module
  { moduleName :: !Text,
    moduleDecls :: [SDecl]
  }
  deriving (Show, Eq)

type SDecl = Spanned Decl

data Decl
  = DeclImport ![Ident]
  | DeclExport !Ident
  | DeclClassDecl SClassDecl
  | DeclClassDef SClassDef
  | DeclInstanceDecl SInstancDecl
  | DeclRecordDef SRecordDef
  deriving (Show, Eq)

type SRecordDef = Spanned RecordDef

data RecordDef = RecordDef
  { recordName :: !Ident,
    recordTypeParams :: ![Ident],
    recordFields :: [(Ident, SAnno)]
  }
  deriving (Show, Eq)

type SClassDef = Spanned ClassDef

data ClassDef
  = ClassDef
  { className :: !Ident,
    classSuperclasses :: ![Ident],
    classTypeParams :: ![Ident],
    classDecls :: [SClassDecl]
  }
  deriving (Show, Eq)

type SClassDecl = Spanned ClassDecl

data ClassDecl
  = ClassDeclSig SSig
  | ClassDeclBind SBind
  deriving (Show, Eq)

type SInstancDecl = Spanned InstanceDecl

data InstanceDecl = InstanceDecl
  { instanceClass :: !Ident,
    instanceTypeParams :: ![Ident],
    instanceAnno :: SAnno,
    instanceDecls :: [SClassDecl]
  }
  deriving (Show, Eq)

type SSig = Spanned Sig

data Sig = Sig
  { sigNames :: ![Ident],
    sigTypeParams :: ![Ident],
    sigAnno :: SAnno
  }
  deriving (Show, Eq)

data Rhs = RhsExpr SExpr | RhsGuard [SGuard]
  deriving (Show, Eq)

type SExpr = Spanned Expr

data Expr
  = Lit !Lit
  | Var !Ident
  | App SExpr SExpr
  | Lam [SPat] SExpr
  | Let [SBind] SExpr
  | Do [SExpr]
  | Neg SExpr
  | Binary !Ident SExpr SExpr
  | If SExpr SExpr SExpr
  | Match SExpr [(SPat, SExpr)]
  | List [SExpr]
  | Tuple [SExpr]
  | Cons Ident [SExpr]
  | Record (Maybe Ident) [(Ident, SExpr)]
  | RecordAccess SExpr Ident
  | RecordUpdate SExpr [(Ident, SExpr)]
  | Unit
  deriving (Show, Eq)

type SBind = Spanned Bind

data Bind
  = BindPat !SPat Rhs [SClassDecl]
  | BindFun !Ident [SAlt] [SClassDecl]
  deriving (Show, Eq)

type SAlt = Spanned Alt

data Alt = Alt
  { altPats :: [SPat],
    altExpr :: Rhs
  }
  deriving (Show, Eq)

type SGuard = Spanned Guard

data Guard = Guard
  { guardPat :: SPat,
    guardExpr :: SExpr
  }
  deriving (Show, Eq)

type SAnno = Spanned Anno

data Anno
  = AnnoVar !Ident
  | AnnoIdent !Ident
  | AnnoFun !SAnno !SAnno
  | AnnoList !SAnno
  | AnnoTuple [SAnno]
  | AnnoUnit
  deriving (Show, Eq)

type SPat = Spanned Pat

data Pat
  = PatWildcard
  | PatLit !Lit
  | PatIdent !Ident
  | PatCons !Ident [SPat]
  | PatAs !Ident SPat
  | PatList [SPat]
  | PatTuple [SPat]
  | PatUnit
  deriving (Show, Eq)

newtype Ident = Ident (Spanned Text) deriving (Show, Eq, Ord)

data Lit
  = Int Integer
  | Bool !Bool
  | String !Text
  deriving (Show, Eq)
