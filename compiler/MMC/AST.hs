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
  = ImportD !Ident
  | ExportD !Ident
  | CDeclD !CDecl
  deriving (Show, Eq)

data ClassDef
  = ClassDef
  { className :: Ident,
    classSuperclasses :: [Ident],
    classTypeParams :: [Ident],
    classMethods :: [LCDecl]
  }
  deriving (Show, Eq)

type LCDecl = Located CDecl

data CDecl
  = SigCD !Ident LTypeAnno
  | DefCD LDef
  deriving (Show, Eq)

type LDef = Located Def

data Def
  = Def
  { defBind :: Bind,
    defBody :: LExpr,
    defWhereBinds :: [LCDecl]
  }
  deriving (Show, Eq)

type LExpr = Located Expr

data Expr
  = LitE !Lit
  | VarE !Ident
  | AppE LExpr LExpr
  | LamE [LPattern] LExpr
  | LetE Bind LExpr LExpr
  | UnaryE !LUnaryOp LExpr
  | BinaryE !LBinaryOp LExpr LExpr
  | IfE LExpr LExpr LExpr
  | MatchE LExpr [(LPattern, LExpr)]
  | ListE [LExpr]
  | TupleE [LExpr]
  | RecordE (Maybe Ident) [(Ident, LExpr)]
  | RecordAccessE LExpr Ident
  | RecordUpdateE LExpr [(Ident, LExpr)]
  | UnitE
  deriving (Show, Eq)

data Bind
  = PatternB !LPattern
  | FunB !Ident [LPattern]
  deriving (Show, Eq)

data MatchGroup = MatchGroup
  { matchGroupScrutinee :: LExpr,
    matchGroupAlts :: [(LPattern, LExpr)]
  }
  deriving (Show, Eq)

data Guard = Guard
  { guardPattern :: LPattern,
    guardExpr :: LExpr
  }
  deriving (Show, Eq)

type LUnaryOp = Located UnaryOp

data UnaryOp
  = NegUO
  deriving (Show, Eq)

unOpName :: UnaryOp -> Text
unOpName NegUO = "neg"

type LBinaryOp = Located BinaryOp

data BinaryOp
  = AddBO
  | SubBO
  | MulBO
  | DivBO
  | ModBO
  | EqBO
  | NeqBO
  deriving (Show, Eq)

binOpName :: BinaryOp -> Text
binOpName AddBO = "add"
binOpName SubBO = "sub"
binOpName MulBO = "mul"
binOpName DivBO = "div"
binOpName ModBO = "mod"
binOpName EqBO = "eq"
binOpName NeqBO = "neq"

type LTypeAnno = Located TypeAnno

data TypeAnno
  = VarTA !Ident
  | IdentTA !Ident
  | FunTA !LTypeAnno !LTypeAnno
  | ListTA !LTypeAnno
  | TupleTA [LTypeAnno]
  | UnitTA
  deriving (Show, Eq)

type LPattern = Located Pattern

data Pattern
  = WildcardP
  | LitP !Lit
  | IdentP !Ident
  | ConsP !Ident [LPattern]
  | AsP !Ident LPattern
  | ListP [LPattern]
  | TupleP [LPattern]
  | UnitP
  deriving (Show, Eq)

newtype Ident = Ident (Located Text) deriving (Show, Eq, Ord)

data Lit
  = IntL Integer
  | BoolL !Bool
  | StringL !Text
  deriving (Show, Eq)
